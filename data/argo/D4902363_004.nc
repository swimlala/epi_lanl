CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T06:44:22Z creation;2016-06-24T06:44:24Z conversion to V3.1;2019-12-19T08:38:59Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160624064422  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_004                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ײ8��.�1   @ײ9Q�n @;�rGE8��da}�H�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A!��AA��A`  A�  A�33A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D~��D� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�z�@�{@�{A ��A@��A_
=A
=A��RA��RA��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�B��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D	�D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D>�D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^��D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh��Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~��D|)D�)D�>D�~D��D��D�>D�~D��D���D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�z�DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D��HD�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D��{D��HD��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�Q�A�S�A�VA�\)A�\)A�M�A�5?A��`A�bNA���A��^A��A�r�A��A�^5A� �A�&�A�S�A��A�|�A���A�p�A�ZA�5?A��/A�A�A��^A�E�A��A�bA�M�A���A�?}A�`BA��A�$�A�5?A�ȴA��mA��A���A��HA�bNA�/A�p�A��!A�(�A�
=A�l�A���A���A�A��uA���A��7A�?}A���A���A��A��A�K�A�/A��9A�O�A�A��A�t�A��jA�S�A�z�A��#A��A�ffA�ĜA�t�A�\)A�K�A�&�A��mA���A��A�1'A��-A�z�A���A�+A��jA�$�A��uA�XA�%A��/A�{A�z�A��A�^5A�7LA�1A��mA�JA��A�I�A���A���A���A���A�/A��HA���A��hA���A�\)A�oA�A~$�A}|�A|�/A{��A{`BAz(�Ay;dAw�
Av1'AtI�AsC�Ar$�Ap��Ao��An(�Am
=AlVAk�AjjAjbAi�Ahn�Ag�PAfv�Af �Ad��Ac��Ab��Aa�FA`�uA_��A^��A]��A\ffA[AZ��AW�AV��AU�AU
=ATJAS%AR�\AQ�-AQXAP�AP�+APA�AO�-ANQ�AM�AL �AK��AK�AJ�!AJ��AJbNAJAI�#AI��AIp�AI;dAH�AHQ�AG�FAGS�AF1'AE\)AEoADVAD$�ADbACAAA@�9A@1A?C�A=7LA<n�A<�9A;A;\)A:A8�A8�A7
=A6JA5��A5XA4�DA3�wA2�+A/��A.$�A,�jA,r�A, �A+�wA+�A+oA)�A(r�A&��A&  A%�mA%?}A$1'A#�A#�7A"�!A!�A!�^A!oA (�At�A�A��A�PAQ�A��Al�A7LA�A��An�A�A��A�A;dA��A1'A�AhsA��A^5AJAn�AAVA��A��A��A��A��AE�A�A
ffA	ƨA	+A�HA$�A�AI�A33A�\A1AhsAA�+A�wA�A%A n�@��@�j@�\)@���@�@�V@�;d@��@�o@�!@��@�9X@��#@�@���@�$�@��@�G�@�Z@��@�C�@�J@�`B@ߕ�@�j@ۮ@��@���@؃@�z�@��@��@�C�@�5?@���@�z�@��
@�C�@��@��@�v�@�I�@őh@�(�@�t�@�;d@��@�(�@�dZ@�33@��@�M�@��h@��/@�1@��P@�?}@�A�@��@�1'@��F@��+@��@�l�@��!@�V@��T@��9@�A�@�|�@��@�&�@���@��;@�S�@�~�@���@�hs@�/@��j@��D@�j@�9X@�1'@� �@��w@�"�@�{@��@��^@�A�@�+@��\@��u@�j@�1@���@�J@�x�@�V@�Ĝ@�9X@���@���@�~�@��T@�X@�1@��w@���@�"�@���@���@�&�@�z�@�@�E�@��@�/@��j@�r�@�Z@�Z@�Q�@�A�@��;@��w@�|�@�+@�ȴ@�~�@�ff@�V@��@��h@�&�@�Ĝ@�bN@�(�@�;d@���@�^5@�J@��7@�x�@�hs@�%@���@���@��j@��@�Z@���@�|�@���@�V@��T@�7L@���@��9@�bN@��
@�l�@���@��@���@���@�$�@���@�@��^@��^@���@��h@�x�@�G�@�?}@��@���@�j@�bN@�Z@�9X@�@;d@~�@~�+@~5?@~@}O�@|��@|I�@{�F@{t�@z�@zn�@y�#@y��@yX@x�u@x1'@wl�@v�@vV@u@u�-@u�-@u�-@uO�@u�@u�@uV@t��@tz�@tj@t1@s�m@s��@rM�@q�^@qG�@p��@p�@o��@n�@m�h@l�@lZ@l1@k�
@k��@kdZ@k33@ko@j�H@jn�@j-@j-@j�@jJ@ix�@i7L@i%@h��@hA�@g��@gl�@fȴ@f�@f�@f�@fE�@e��@eV@d�j@dZ@cƨ@b�!@b=q@b-@b�@b�@a�@a�^@ax�@`��@_�w@_|�@_K�@_�@^��@^�R@^��@^V@]�@]�h@]p�@]O�@]V@\��@\(�@\1@\1@\1@\1@\1@[�m@[ƨ@[33@Z��@Z~�@ZJ@Y�^@Y��@Y�^@Y�7@Y7L@X�`@X��@X�@XbN@XbN@XQ�@X1'@X  @X  @W�@W�;@W��@W�@W|�@W;d@V��@V��@V@U�@Up�@Up�@Up�@Up�@U�@TI�@S�F@S"�@R�!@R~�@RM�@R-@R-@R-@R�@RJ@Q�@Q�^@Q��@Q��@QX@Q7L@Q�@PĜ@P��@PQ�@O�@O�@O|�@O\)@OK�@O
=@N��@M��@L�@L�D@LI�@K�m@K"�@J��@J��@J�\@J�\@J�\@J~�@J^5@J=q@JJ@I��@I7L@HĜ@HbN@G+@F$�@E��@EV@D�@DZ@D�@C��@Cƨ@C��@CC�@Co@C@B�H@B��@B��@Bn�@BM�@BJ@A��@@Ĝ@@Q�@@1'@@ �@?��@?|�@>��@>�@>�R@>@=�T@=@=p�@=O�@=�@<��@<�D@<z�@<Z@<Z@<I�@<9X@<9X@<�@;�m@;S�@;"�@;@:��@:��@:�\@:M�@:�@9�@9x�@9G�@97L@9�@9%@8�@81'@7�@7�@7;d@6�R@6ff@6ff@6V@6E�@6E�@65?@6$�@5��@5p�@4�@4�@4�D@3�m@3dZ@3C�@3"�@2�@2��@2^5@1G�@1%@0�`@0�9@0r�@0A�@/�;@/�w@/�P@/�P@/|�@/\)@/�@.�y@.�R@.�+@.ff@.V@.5?@.{@-�T@-�-@-p�@-?}@-V@,��@,Z@,�@+�
@+�F@+��@+t�@+C�@+"�@*�H@*~�@*^5@*=q@*-@)�@)�#@)��@)x�@)7L@(�9@(r�@(Q�@(A�@(b@(  @'l�@';d@'+@'
=@&�@&�R@&��@&v�@&V@&5?@&{@%�@%�T@%��@%@%@%�-@%�h@%p�@%`B@%?}@%/@%V@$j@#�m@#ƨ@#�F@#�F@#��@#�@#t�@#t�@#t�@#t�@#C�@#33@#"�@"��@"�!@"�\@"n�@"^5@"�@!�#@!x�@!G�@!%@ ��@ �u@ bN@ A�@ 1'@ b@�@�@\)@�@
=@
=@
=@�y@�y@�@ȴ@��@v�@ff@5?@$�@��@�-@��@�@`B@�@�/@��@��@��@��@��@Z@�@1@1@��@�
@S�@o@��@��@n�@M�@�@J@��@��@�@�#@��@��@�^@��@��@��@�7@hs@G�@��@��@�u@bN@A�@ �@  @�;@��@�w@�w@��@��@|�@l�@K�@;d@��@v�@V@V@E�@E�@�@�@�@�@�/@��@��@Z@�@��@�
@�F@dZ@"�@@��@��@~�@J@��@�7@x�@X@Ĝ@Q�@b@  @�;@�w@�@�P@\)@K�@+@�y@�R@v�@V@5?@$�@�T@�-@p�@/@V@�/@I�@�
@�F@�@t�@dZ@33@
��@
�!@
�!@
�!@
��@
�\@
~�@
^5@
M�@
M�@
�@	�#@	��@	��@	X@�`@Ĝ@��@�u@r�@Q�@1'@�w@l�@K�@K�@;d@+@�@
=@��@�y@�@ȴ@��@�+@V@V@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�Q�A�S�A�VA�\)A�\)A�M�A�5?A��`A�bNA���A��^A��A�r�A��A�^5A� �A�&�A�S�A��A�|�A���A�p�A�ZA�5?A��/A�A�A��^A�E�A��A�bA�M�A���A�?}A�`BA��A�$�A�5?A�ȴA��mA��A���A��HA�bNA�/A�p�A��!A�(�A�
=A�l�A���A���A�A��uA���A��7A�?}A���A���A��A��A�K�A�/A��9A�O�A�A��A�t�A��jA�S�A�z�A��#A��A�ffA�ĜA�t�A�\)A�K�A�&�A��mA���A��A�1'A��-A�z�A���A�+A��jA�$�A��uA�XA�%A��/A�{A�z�A��A�^5A�7LA�1A��mA�JA��A�I�A���A���A���A���A�/A��HA���A��hA���A�\)A�oA�A~$�A}|�A|�/A{��A{`BAz(�Ay;dAw�
Av1'AtI�AsC�Ar$�Ap��Ao��An(�Am
=AlVAk�AjjAjbAi�Ahn�Ag�PAfv�Af �Ad��Ac��Ab��Aa�FA`�uA_��A^��A]��A\ffA[AZ��AW�AV��AU�AU
=ATJAS%AR�\AQ�-AQXAP�AP�+APA�AO�-ANQ�AM�AL �AK��AK�AJ�!AJ��AJbNAJAI�#AI��AIp�AI;dAH�AHQ�AG�FAGS�AF1'AE\)AEoADVAD$�ADbACAAA@�9A@1A?C�A=7LA<n�A<�9A;A;\)A:A8�A8�A7
=A6JA5��A5XA4�DA3�wA2�+A/��A.$�A,�jA,r�A, �A+�wA+�A+oA)�A(r�A&��A&  A%�mA%?}A$1'A#�A#�7A"�!A!�A!�^A!oA (�At�A�A��A�PAQ�A��Al�A7LA�A��An�A�A��A�A;dA��A1'A�AhsA��A^5AJAn�AAVA��A��A��A��A��AE�A�A
ffA	ƨA	+A�HA$�A�AI�A33A�\A1AhsAA�+A�wA�A%A n�@��@�j@�\)@���@�@�V@�;d@��@�o@�!@��@�9X@��#@�@���@�$�@��@�G�@�Z@��@�C�@�J@�`B@ߕ�@�j@ۮ@��@���@؃@�z�@��@��@�C�@�5?@���@�z�@��
@�C�@��@��@�v�@�I�@őh@�(�@�t�@�;d@��@�(�@�dZ@�33@��@�M�@��h@��/@�1@��P@�?}@�A�@��@�1'@��F@��+@��@�l�@��!@�V@��T@��9@�A�@�|�@��@�&�@���@��;@�S�@�~�@���@�hs@�/@��j@��D@�j@�9X@�1'@� �@��w@�"�@�{@��@��^@�A�@�+@��\@��u@�j@�1@���@�J@�x�@�V@�Ĝ@�9X@���@���@�~�@��T@�X@�1@��w@���@�"�@���@���@�&�@�z�@�@�E�@��@�/@��j@�r�@�Z@�Z@�Q�@�A�@��;@��w@�|�@�+@�ȴ@�~�@�ff@�V@��@��h@�&�@�Ĝ@�bN@�(�@�;d@���@�^5@�J@��7@�x�@�hs@�%@���@���@��j@��@�Z@���@�|�@���@�V@��T@�7L@���@��9@�bN@��
@�l�@���@��@���@���@�$�@���@�@��^@��^@���@��h@�x�@�G�@�?}@��@���@�j@�bN@�Z@�9X@�@;d@~�@~�+@~5?@~@}O�@|��@|I�@{�F@{t�@z�@zn�@y�#@y��@yX@x�u@x1'@wl�@v�@vV@u@u�-@u�-@u�-@uO�@u�@u�@uV@t��@tz�@tj@t1@s�m@s��@rM�@q�^@qG�@p��@p�@o��@n�@m�h@l�@lZ@l1@k�
@k��@kdZ@k33@ko@j�H@jn�@j-@j-@j�@jJ@ix�@i7L@i%@h��@hA�@g��@gl�@fȴ@f�@f�@f�@fE�@e��@eV@d�j@dZ@cƨ@b�!@b=q@b-@b�@b�@a�@a�^@ax�@`��@_�w@_|�@_K�@_�@^��@^�R@^��@^V@]�@]�h@]p�@]O�@]V@\��@\(�@\1@\1@\1@\1@\1@[�m@[ƨ@[33@Z��@Z~�@ZJ@Y�^@Y��@Y�^@Y�7@Y7L@X�`@X��@X�@XbN@XbN@XQ�@X1'@X  @X  @W�@W�;@W��@W�@W|�@W;d@V��@V��@V@U�@Up�@Up�@Up�@Up�@U�@TI�@S�F@S"�@R�!@R~�@RM�@R-@R-@R-@R�@RJ@Q�@Q�^@Q��@Q��@QX@Q7L@Q�@PĜ@P��@PQ�@O�@O�@O|�@O\)@OK�@O
=@N��@M��@L�@L�D@LI�@K�m@K"�@J��@J��@J�\@J�\@J�\@J~�@J^5@J=q@JJ@I��@I7L@HĜ@HbN@G+@F$�@E��@EV@D�@DZ@D�@C��@Cƨ@C��@CC�@Co@C@B�H@B��@B��@Bn�@BM�@BJ@A��@@Ĝ@@Q�@@1'@@ �@?��@?|�@>��@>�@>�R@>@=�T@=@=p�@=O�@=�@<��@<�D@<z�@<Z@<Z@<I�@<9X@<9X@<�@;�m@;S�@;"�@;@:��@:��@:�\@:M�@:�@9�@9x�@9G�@97L@9�@9%@8�@81'@7�@7�@7;d@6�R@6ff@6ff@6V@6E�@6E�@65?@6$�@5��@5p�@4�@4�@4�D@3�m@3dZ@3C�@3"�@2�@2��@2^5@1G�@1%@0�`@0�9@0r�@0A�@/�;@/�w@/�P@/�P@/|�@/\)@/�@.�y@.�R@.�+@.ff@.V@.5?@.{@-�T@-�-@-p�@-?}@-V@,��@,Z@,�@+�
@+�F@+��@+t�@+C�@+"�@*�H@*~�@*^5@*=q@*-@)�@)�#@)��@)x�@)7L@(�9@(r�@(Q�@(A�@(b@(  @'l�@';d@'+@'
=@&�@&�R@&��@&v�@&V@&5?@&{@%�@%�T@%��@%@%@%�-@%�h@%p�@%`B@%?}@%/@%V@$j@#�m@#ƨ@#�F@#�F@#��@#�@#t�@#t�@#t�@#t�@#C�@#33@#"�@"��@"�!@"�\@"n�@"^5@"�@!�#@!x�@!G�@!%@ ��@ �u@ bN@ A�@ 1'@ b@�@�@\)@�@
=@
=@
=@�y@�y@�@ȴ@��@v�@ff@5?@$�@��@�-@��@�@`B@�@�/@��@��@��@��@��@Z@�@1@1@��@�
@S�@o@��@��@n�@M�@�@J@��@��@�@�#@��@��@�^@��@��@��@�7@hs@G�@��@��@�u@bN@A�@ �@  @�;@��@�w@�w@��@��@|�@l�@K�@;d@��@v�@V@V@E�@E�@�@�@�@�@�/@��@��@Z@�@��@�
@�F@dZ@"�@@��@��@~�@J@��@�7@x�@X@Ĝ@Q�@b@  @�;@�w@�@�P@\)@K�@+@�y@�R@v�@V@5?@$�@�T@�-@p�@/@V@�/@I�@�
@�F@�@t�@dZ@33@
��@
�!@
�!@
�!@
��@
�\@
~�@
^5@
M�@
M�@
�@	�#@	��@	��@	X@�`@Ĝ@��@�u@r�@Q�@1'@�w@l�@K�@K�@;d@+@�@
=@��@�y@�@ȴ@��@�+@V@V@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�1B�7B�7B�7B�7B�7B�1B�%B��Bl�B�=B�7B�7B�B�Bp�B\)BR�BL�BR�BW
BW
BZB\)B\)B^5B^5B\)BZBS�BL�B?}B/B"�B�BJB��B�B�B�B�ZB�5B�B��B��B��B��BĜB��B�dB�FB�FB�-B�B�B��B��B��B��B��B�oB�VB�DB�B�B|�Bt�Bo�BdZB`BBXBN�BJ�BF�B?}B9XB7LB6FB49B0!B,B$�B�B
=B%B��B�B�HB�
BȴBÖB�qB�XB�B��B��B�\B�DB�1B�By�BbNBVBI�B<jB7LB2-B)�BbB
�B
��B
�wB
�9B
�B
��B
�1B
�B
{�B
r�B
m�B
e`B
[#B
P�B
C�B
49B
,B
%�B
�B
�B

=B
  B	��B	�B	�B	�B	�B	�B	�sB	�NB	�BB	�B	��B	ɺB	ƨB	��B	�wB	�LB	�!B	��B	��B	��B	��B	�=B	�+B	�B	}�B	x�B	u�B	r�B	o�B	m�B	l�B	k�B	k�B	gmB	cTB	`BB	\)B	ZB	W
B	W
B	VB	T�B	T�B	S�B	Q�B	Q�B	N�B	L�B	J�B	H�B	E�B	@�B	?}B	<jB	:^B	8RB	6FB	,B	"�B	�B	oB	B��B	DB	1B	
=B	1B��B��B�B�`B�HB�BB�5B�B��BĜB�wB�RB�FB�9B�3B�'B�!B�B��B��B��B��B��B��B�{B�uB�uB�hB�bB�\B�=B�B�B�B~�Bx�B�B�%B�B�B�B�B�B� B� B~�B{�Bv�Br�Bm�B\)BYBYBW
BS�BQ�BQ�BQ�BP�BP�BL�BJ�BI�BG�BF�BD�BB�BB�B>wB=qB;dB:^B8RB7LB6FB5?B33B2-B2-B/B0!B,B,B+B)�B(�B(�B&�B$�B#�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B!�B!�B"�B%�B%�B%�B%�B'�B)�B)�B(�B+B/B0!B0!B0!B33B49B7LB8RB9XB9XB:^B<jB=qB?}BE�BI�BR�BYB[#B_;BaHBbNBbNBcTBcTBdZBdZBdZBe`BffBhsBm�Bm�Bn�Bt�Bw�Bz�B|�B~�B�B�B�B�%B�+B�1B�7B�PB�PB�VB�bB�oB��B��B��B��B��B��B��B��B�'B�?B�FB�dB�qB�}B��B��B��B��BÖBÖBŢBǮBɺB��B��B��B��B��B��B�B�#B�)B�BB�BB�HB�TB�`B�`B�`B�fB�sB�B�B�B�B�B��B��B��B��B��B	B	B	+B	1B	
=B	PB	VB	VB	VB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	"�B	$�B	'�B	)�B	+B	,B	-B	0!B	33B	49B	6FB	7LB	:^B	<jB	?}B	?}B	@�B	C�B	E�B	I�B	K�B	N�B	Q�B	Q�B	Q�B	Q�B	T�B	VB	VB	VB	XB	YB	ZB	\)B	]/B	^5B	bNB	e`B	gmB	hsB	k�B	l�B	o�B	t�B	w�B	z�B	}�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�JB	�PB	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�9B	�9B	�?B	�FB	�LB	�^B	�qB	�wB	�}B	�}B	��B	��B	B	ÖB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�#B	�/B	�5B	�;B	�BB	�HB	�HB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
+B
1B
1B
	7B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
PB
VB
\B
bB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
-B
.B
.B
.B
.B
.B
/B
0!B
1'B
2-B
2-B
33B
33B
33B
49B
49B
49B
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
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
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
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
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
W
B
XB
XB
XB
XB
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
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
[#B
\)B
]/B
]/B
\)B
\)B
\)B
]/B
]/B
^5B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
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
n�B
n�B
n�B
n�B
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
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�1B�7B�7B�7B��B�	B��B��B�jBn}B�#B��B�#B��B��Bt�B^jBT�BN�BUBX�BWsBZ�B\�B]/B_�B_pB]IB[WBVBO(BCB2GB%zB�B(B��B�B�B�CB�B߾B�1B��BңB�bB��B�SB��B��B�LB��B�MB�OB��B��B��B��B�'B�kB�B��B�dB�%B��B}�Bu�Bq'Be�BbBYKBOvBK�BG�B@B9�B7�B6�B4�B0�B-�B&�B�BBzB��B��B�B�+B�RB�gB�BB��B��B�,B��B��B��B�B�YB|6Bc�BW�BKB="B88B3�B-wB�B
�B
ңB
��B
�tB
�iB
�xB
�B
�B
}B
s�B
o B
f�B
\�B
R�B
E�B
5�B
-wB
'mB
5B
sB
�B
 B	�PB	�B	�hB	��B	��B	��B	�B	�:B	��B	�kB	�TB	�^B	�1B	��B	��B	��B	��B	�
B	��B	��B	�
B	�DB	�fB	�MB	.B	y�B	v�B	s3B	p;B	nB	m)B	l�B	m)B	iB	dtB	`�B	\�B	Z�B	WYB	W�B	VmB	UgB	UMB	TFB	RoB	R�B	OvB	M�B	K�B	J	B	F�B	A B	@OB	<�B	:�B	9�B	7�B	-wB	#�B		B	{B	�B�<B	JB		7B	�B		�B��B�B��B��B��B�|BߤB�7B�9BƨB� B��B��B��B��B�GB�B��B��B��B�5B��B��B�?B�MB��B�aB�B�hB�}B�)B��B��B��B�OBy�B��B�tB�mB��B��B�B�;B�iB��B�B}BxlBu�Bp;B]BZBZ�BXBT�BR:BRTBR�BR BR BNBLJBJ�BH�BG�BESBC�BC�B?�B>�B<PB;0B9>B8B7B6FB3�B33B3MB1B1'B,�B,�B+�B*�B*eB*�B'�B%zB$�B%,B$ZB"�B �B 'B�B�BdB)BWB�BqB#B~BWBkB�B	B#B�BBB�BCB�B/BBBB�BOB!�B#�B#TB"hB"�B$B&fB&2B&LB&�B(�B*�B*�B)�B,�B/�B0UB0UB0�B49B5�B8B8�B9�B9�B;JB<�B>(B@�BFYBJXBS�BY�B[�B_�Ba�Bb�Bb�Bc�Bc�Bd�Bd�Bd�Be�BgBiDBm�Bn/Bo�Bu�Bx�B|B}<B}B�B��B��B��B��B��B�#B��B��B��B� B�@B��B��B�B�CB�jB�TB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�6B�.B�oB�MB�_B�qB��B�B��B�B�B�B�B��B�B�B�B��B��B�B�B�ZB�$B�DB�jB�BB	UB	aB	�B	�B	
�B	jB	pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 �B	 �B	"B	# B	%FB	(>B	*0B	+QB	,WB	-]B	0oB	3�B	4�B	6zB	7�B	:�B	<�B	?�B	?�B	@�B	C�B	FB	I�B	LB	OB	Q�B	R B	RB	R B	UB	VB	VB	V9B	XEB	Y1B	ZQB	\CB	]dB	^�B	b�B	e�B	g�B	h�B	k�B	mB	pB	u%B	xB	z�B	~B	.B	�AB	�-B	�9B	�9B	�YB	�KB	�=B	�^B	�~B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�>B	�qB	�AB	�3B	�9B	�nB	�TB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�4B	�&B	�@B	�&B	�B	�B	�
B	�?B	�WB	�IB	�OB	�pB	�\B	�HB	�|B	�nB	�tB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	�B	�B	�B	�B	�.B	�HB
 B
'B
GB
3B
3B
gB
gB
mB
_B
fB
KB
	lB

�B
xB
^B
JB
JB
dB
dB
~B
dB
dB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
�B
�B
 B
 �B
!�B
"�B
#�B
#�B
#�B
$B
$�B
%B
$�B
$�B
$�B
$�B
%B
$�B
%B
%�B
%�B
%�B
%�B
'B
'B
'B
(
B
($B
)B
*0B
*B
*B
*KB
+B
+6B
+B
+6B
,WB
,"B
-)B
-)B
.B
./B
./B
./B
.IB
/OB
0UB
1AB
2GB
2aB
3hB
3MB
3MB
4TB
4TB
4nB
6�B
7fB
7�B
7fB
7�B
8lB
9rB
9rB
:xB
:^B
:�B
:xB
:xB
:xB
;�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
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
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
J	B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
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
L�B
L�B
L�B
M�B
NB
NB
N�B
N�B
N�B
OB
N�B
N�B
PB
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q B
Q B
Q B
QB
R B
RB
R B
Q�B
RB
R B
S&B
SB
R�B
SB
SB
R�B
SB
TB
T,B
S�B
S�B
TB
TB
TFB
UB
UB
VB
VB
VB
W$B
XB
XB
XB
XB
X+B
XB
Y1B
Y1B
YB
Y1B
Y1B
Y1B
YKB
Y1B
Y1B
ZQB
Z7B
ZQB
ZQB
Z7B
[=B
[=B
[=B
[#B
[#B
[WB
[#B
[=B
\)B
\]B
\CB
[qB
\]B
]IB
]/B
\)B
\]B
\xB
]dB
]dB
^jB
]IB
]dB
]IB
^OB
^jB
^jB
^OB
_VB
_VB
_pB
_pB
_VB
`vB
`vB
`vB
abB
a|B
bhB
b�B
b�B
c�B
dtB
dtB
dtB
ezB
e`B
ezB
e�B
ezB
ezB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
i�B
j�B
jB
jB
jB
j�B
jB
j�B
jB
k�B
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
n�B
n�B
n�B
n�B
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
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606120036162016061200361620160612003616201806221138532018062211385320180622113853201804050401242018040504012420180405040124  JA  ARFMdecpA19c                                                                20160624153519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624064422  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624064422  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624064423  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624064423  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624064423  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624064424  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624064424  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160624064424  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160624064424                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624071803                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160608002237  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160608002237  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160608002237  CV  LATITUDE        G�O�G�O�A�`B                JM  ARGQJMQC2.0                                                                 20160608002237  CV  LONGITUDE       G�O�G�O��#�                JM  ARCAJMQC2.0                                                                 20160611153616  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160611153616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190124  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622023853  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                