CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-20T00:35:32Z creation;2016-11-20T00:35:35Z conversion to V3.1;2019-12-19T08:25:04Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20161120003532  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ;A   JA  I2_0576_059                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��tmj��1   @��u""" @:��Mj�d��ߤ?�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @|(�@��H@�{A ��A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_Bh(�BoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�AHD�~D׾D���D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�p�A�n�A�p�A�v�A�t�A�v�A�v�A�v�A�x�A�x�A�z�A�z�A�z�A�|�A�z�A�|�A�~�A΁A΁A΁A΃A΃A΃A΅A΅A΅A΅A·+A·+A·+A·+A·+AΉ7A΋DA΋DA·+A΃A�z�A�t�A�t�A�bNA���A��Aǉ7A�"�A�n�A�\)A�n�A�z�A�{A�l�A�A�1A��A�jA�I�A�VA��A���A��#A�A�K�A�r�A�bNA���A�(�A���A��#A�r�A��A��A�ĜA�n�A� �A���A�ĜA��!A���A��hA�7LA��9A�p�A��`A��A��A��A���A�z�A�p�A���A��jA���A��RA�`BA��9A�E�A���A�C�A�`BA���A���A��A�C�A��/A�t�A���A���A�ffA���A�"�A��/A�XA���A� �A���A�S�A�VA��TA��-A���A��7A��HA�
=A� �A~��A}C�A{&�Ax��Au��As�^As"�ArE�Aq?}An1'Al��Ak��Aj1Af�`Ad�jAd �AcG�Ab�RAb��AaG�A`��A`�\A`Q�A_VA]%A[�^AZQ�AYp�AX�AX�AVbNAT�DASC�AR�+AQhsAOAK��AJ��AJJAG33AFI�AE��AE�AE&�AC�wABJAB1AAAA\)AAC�AA�A@�A?�A=�A=?}A;�PA:��A:~�A8�DA5�#A4�jA4JA3+A2r�A1�;A1dZA0�A0bNA.��A-��A-;dA,r�A,$�A+��A+%A*��A(�+A'XA&ĜA&M�A&-A%�FA$�9A#oA!%A�
A5?A��A�AbNA��AjA$�A�TA�FA��At�A?}AO�A%AAK�AȴAr�AJAAG�A^5A%A�9A�TA�yA�\A(�A��A7LA~�AS�A��A
��A	�wA��A��A��A��AE�AVAz�AA��A�A ^5@�C�@�@���@��@�r�@�+@�-@���@�bN@�@�@�S�@@��@�P@�R@�p�@�@�ff@�p�@�u@��m@�S�@�!@��@� �@�K�@��y@�J@���@�\)@ڸR@��@���@��T@Ұ!@�@�A�@�-@́@�p�@�/@�(�@��y@�J@ɩ�@�A�@Ɵ�@ź^@�(�@�M�@��@��@���@�;d@��R@�n�@�J@���@�$�@�J@��@��@�dZ@��@�M�@���@�%@��@�%@�z�@�@�hs@�A�@���@�S�@�
=@�@�&�@��@� �@�\)@��@�V@��@�+@��+@�@�X@�V@�Q�@��@���@��h@�O�@��@�r�@��@��@�{@�@��7@��`@�j@�  @���@�33@��+@�{@��-@��j@���@��@�C�@�33@�V@�-@��@���@�hs@�X@�/@��D@��;@��@�dZ@�"�@��@�M�@���@�x�@��@�Ĝ@�Z@���@�"�@�~�@�@�@��@���@�O�@���@��D@���@��u@�Q�@�1'@�1@��@��m@�ƨ@��F@��@��@�o@���@��\@�E�@�{@���@���@���@��;@��H@���@��!@��R@���@���@���@���@���@�v�@�ff@�E�@�5?@�$�@��@�@��@���@��7@�`B@���@��D@�1'@�  @��@���@�t�@�K�@��y@���@�~�@�{@���@��^@��-@���@���@�X@�7L@��/@�z�@�9X@��@�;@�P@|�@;d@~ȴ@~��@~��@~5?@}�@}`B@}�@|Z@|1@{�
@{C�@z�!@zJ@y&�@xb@w|�@v��@v��@vff@u`B@t9X@t9X@t��@sdZ@st�@sdZ@r�H@r=q@rn�@r��@r��@r��@s33@st�@s��@s�@sC�@r~�@r=q@q�@q��@q��@qX@pQ�@o�@o�;@o�;@o�w@o�P@o;d@o�@o
=@n��@n�y@nȴ@nȴ@n�R@n��@nE�@mp�@l�/@l�@k�
@j�!@j~�@j~�@i��@iX@i7L@h��@hĜ@h��@hQ�@h  @g�@g�P@g;d@f��@fȴ@f�R@f�@f�R@f�+@fff@f{@e@e`B@e/@eV@d��@dz�@d(�@d�@d1@c�m@c33@b~�@bJ@a��@a�^@ax�@a�@`Ĝ@`r�@`b@_�;@_�@_\)@^�@^��@^5?@]�h@]�@\�D@\�@\(�@\Z@[��@[dZ@Z�H@Zn�@Z=q@Y��@Y�7@YX@Y&�@Y%@Y%@XĜ@X��@Xb@Xb@W�w@W;d@V�+@U��@U��@Up�@U/@T�/@T��@Tz�@TI�@S�m@St�@R�H@Rn�@RM�@R=q@R=q@R=q@R-@RJ@Q�^@QX@P�`@PbN@PQ�@PA�@P �@P  @O�;@O|�@O+@Nv�@N{@M��@Mp�@MO�@L��@Lj@K��@K�F@Kt�@KS�@KS�@K33@J�@J�\@I��@Ix�@I�@H��@H�u@HbN@H  @G��@Gl�@G�@F�R@Fv�@FE�@E�T@E?}@D�@D�/@D�/@D�j@D�@DI�@CdZ@C"�@Co@C@B�@B��@B��@B=q@BJ@A�^@A��@A��@A�7@Ahs@A7L@A&�@@��@@��@@�9@@�@@bN@?�;@?�P@?l�@?K�@?
=@>�y@>ȴ@>��@>ff@>5?@>$�@=��@=`B@<��@<Z@<Z@;�m@;��@;S�@;"�@:�H@:��@:�\@:n�@:M�@:M�@:-@:J@9��@9��@9x�@9X@9%@8�u@8bN@8 �@8  @7;d@6�@6v�@65?@5�T@5��@5O�@5V@4�@4�@4�@4��@4�@4Z@41@3�m@3�F@3��@333@2��@2��@2n�@2�@1�#@1�@0�9@0 �@/��@/+@.ȴ@.��@.�+@.ff@.5?@-��@-�h@-O�@,��@,�j@,�@,�D@,�D@,z�@,z�@,z�@,j@,I�@,�@+��@+ƨ@+��@+�@+t�@+S�@+C�@+33@*�@*��@*^5@)�@)��@)��@)x�@)X@)&�@(��@(bN@'�P@'K�@'
=@&V@&@%��@%�h@%`B@%�@$�@$�@$Z@$�@#�
@#��@#��@#�@#t�@#S�@#"�@#o@"��@"~�@"-@!�#@!�^@!X@!&�@!%@ �u@ bN@ 1'@�@��@|�@l�@�@��@��@V@{@�-@?}@��@Z@��@�@dZ@o@��@��@��@�!@��@�\@^5@J@J@��@�@�7@%@�9@�u@�u@Q�@ �@�@��@;d@�R@E�@@�h@`B@O�@�@�@��@�@��@�D@Z@9X@��@�F@dZ@"�@��@�\@^5@��@X@7L@��@�@bN@1'@b@�@|�@�@v�@E�@V@ff@ff@ff@V@5?@�@�-@�-@�h@`B@V@��@��@�@j@9X@1@�
@��@��@t�@dZ@dZ@dZ@dZ@dZ@C�@o@
��@
�!@
�\@
n�@
-@
-@
=q@
�@	��@	��@	��@	hs@	�@��@Ĝ@�u@�@r�@Q�@A�@  @|�@\)@K�@+@�@��@��@��@ff@E�@$�@{@�@��@@p�@V@��@��@��@Z@I�@9X@�@��@��@�m@�
@�
@�
@ƨ@ƨ@ƨ@ƨ@�F@�F@dZ@o@��@�\@n�@M�@-@�@J@J@��@�@�@�#@�#@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�p�A�n�A�p�A�v�A�t�A�v�A�v�A�v�A�x�A�x�A�z�A�z�A�z�A�|�A�z�A�|�A�~�A΁A΁A΁A΃A΃A΃A΅A΅A΅A΅A·+A·+A·+A·+A·+AΉ7A΋DA΋DA·+A΃A�z�A�t�A�t�A�bNA���A��Aǉ7A�"�A�n�A�\)A�n�A�z�A�{A�l�A�A�1A��A�jA�I�A�VA��A���A��#A�A�K�A�r�A�bNA���A�(�A���A��#A�r�A��A��A�ĜA�n�A� �A���A�ĜA��!A���A��hA�7LA��9A�p�A��`A��A��A��A���A�z�A�p�A���A��jA���A��RA�`BA��9A�E�A���A�C�A�`BA���A���A��A�C�A��/A�t�A���A���A�ffA���A�"�A��/A�XA���A� �A���A�S�A�VA��TA��-A���A��7A��HA�
=A� �A~��A}C�A{&�Ax��Au��As�^As"�ArE�Aq?}An1'Al��Ak��Aj1Af�`Ad�jAd �AcG�Ab�RAb��AaG�A`��A`�\A`Q�A_VA]%A[�^AZQ�AYp�AX�AX�AVbNAT�DASC�AR�+AQhsAOAK��AJ��AJJAG33AFI�AE��AE�AE&�AC�wABJAB1AAAA\)AAC�AA�A@�A?�A=�A=?}A;�PA:��A:~�A8�DA5�#A4�jA4JA3+A2r�A1�;A1dZA0�A0bNA.��A-��A-;dA,r�A,$�A+��A+%A*��A(�+A'XA&ĜA&M�A&-A%�FA$�9A#oA!%A�
A5?A��A�AbNA��AjA$�A�TA�FA��At�A?}AO�A%AAK�AȴAr�AJAAG�A^5A%A�9A�TA�yA�\A(�A��A7LA~�AS�A��A
��A	�wA��A��A��A��AE�AVAz�AA��A�A ^5@�C�@�@���@��@�r�@�+@�-@���@�bN@�@�@�S�@@��@�P@�R@�p�@�@�ff@�p�@�u@��m@�S�@�!@��@� �@�K�@��y@�J@���@�\)@ڸR@��@���@��T@Ұ!@�@�A�@�-@́@�p�@�/@�(�@��y@�J@ɩ�@�A�@Ɵ�@ź^@�(�@�M�@��@��@���@�;d@��R@�n�@�J@���@�$�@�J@��@��@�dZ@��@�M�@���@�%@��@�%@�z�@�@�hs@�A�@���@�S�@�
=@�@�&�@��@� �@�\)@��@�V@��@�+@��+@�@�X@�V@�Q�@��@���@��h@�O�@��@�r�@��@��@�{@�@��7@��`@�j@�  @���@�33@��+@�{@��-@��j@���@��@�C�@�33@�V@�-@��@���@�hs@�X@�/@��D@��;@��@�dZ@�"�@��@�M�@���@�x�@��@�Ĝ@�Z@���@�"�@�~�@�@�@��@���@�O�@���@��D@���@��u@�Q�@�1'@�1@��@��m@�ƨ@��F@��@��@�o@���@��\@�E�@�{@���@���@���@��;@��H@���@��!@��R@���@���@���@���@���@�v�@�ff@�E�@�5?@�$�@��@�@��@���@��7@�`B@���@��D@�1'@�  @��@���@�t�@�K�@��y@���@�~�@�{@���@��^@��-@���@���@�X@�7L@��/@�z�@�9X@��@�;@�P@|�@;d@~ȴ@~��@~��@~5?@}�@}`B@}�@|Z@|1@{�
@{C�@z�!@zJ@y&�@xb@w|�@v��@v��@vff@u`B@t9X@t9X@t��@sdZ@st�@sdZ@r�H@r=q@rn�@r��@r��@r��@s33@st�@s��@s�@sC�@r~�@r=q@q�@q��@q��@qX@pQ�@o�@o�;@o�;@o�w@o�P@o;d@o�@o
=@n��@n�y@nȴ@nȴ@n�R@n��@nE�@mp�@l�/@l�@k�
@j�!@j~�@j~�@i��@iX@i7L@h��@hĜ@h��@hQ�@h  @g�@g�P@g;d@f��@fȴ@f�R@f�@f�R@f�+@fff@f{@e@e`B@e/@eV@d��@dz�@d(�@d�@d1@c�m@c33@b~�@bJ@a��@a�^@ax�@a�@`Ĝ@`r�@`b@_�;@_�@_\)@^�@^��@^5?@]�h@]�@\�D@\�@\(�@\Z@[��@[dZ@Z�H@Zn�@Z=q@Y��@Y�7@YX@Y&�@Y%@Y%@XĜ@X��@Xb@Xb@W�w@W;d@V�+@U��@U��@Up�@U/@T�/@T��@Tz�@TI�@S�m@St�@R�H@Rn�@RM�@R=q@R=q@R=q@R-@RJ@Q�^@QX@P�`@PbN@PQ�@PA�@P �@P  @O�;@O|�@O+@Nv�@N{@M��@Mp�@MO�@L��@Lj@K��@K�F@Kt�@KS�@KS�@K33@J�@J�\@I��@Ix�@I�@H��@H�u@HbN@H  @G��@Gl�@G�@F�R@Fv�@FE�@E�T@E?}@D�@D�/@D�/@D�j@D�@DI�@CdZ@C"�@Co@C@B�@B��@B��@B=q@BJ@A�^@A��@A��@A�7@Ahs@A7L@A&�@@��@@��@@�9@@�@@bN@?�;@?�P@?l�@?K�@?
=@>�y@>ȴ@>��@>ff@>5?@>$�@=��@=`B@<��@<Z@<Z@;�m@;��@;S�@;"�@:�H@:��@:�\@:n�@:M�@:M�@:-@:J@9��@9��@9x�@9X@9%@8�u@8bN@8 �@8  @7;d@6�@6v�@65?@5�T@5��@5O�@5V@4�@4�@4�@4��@4�@4Z@41@3�m@3�F@3��@333@2��@2��@2n�@2�@1�#@1�@0�9@0 �@/��@/+@.ȴ@.��@.�+@.ff@.5?@-��@-�h@-O�@,��@,�j@,�@,�D@,�D@,z�@,z�@,z�@,j@,I�@,�@+��@+ƨ@+��@+�@+t�@+S�@+C�@+33@*�@*��@*^5@)�@)��@)��@)x�@)X@)&�@(��@(bN@'�P@'K�@'
=@&V@&@%��@%�h@%`B@%�@$�@$�@$Z@$�@#�
@#��@#��@#�@#t�@#S�@#"�@#o@"��@"~�@"-@!�#@!�^@!X@!&�@!%@ �u@ bN@ 1'@�@��@|�@l�@�@��@��@V@{@�-@?}@��@Z@��@�@dZ@o@��@��@��@�!@��@�\@^5@J@J@��@�@�7@%@�9@�u@�u@Q�@ �@�@��@;d@�R@E�@@�h@`B@O�@�@�@��@�@��@�D@Z@9X@��@�F@dZ@"�@��@�\@^5@��@X@7L@��@�@bN@1'@b@�@|�@�@v�@E�@V@ff@ff@ff@V@5?@�@�-@�-@�h@`B@V@��@��@�@j@9X@1@�
@��@��@t�@dZ@dZ@dZ@dZ@dZ@C�@o@
��@
�!@
�\@
n�@
-@
-@
=q@
�@	��@	��@	��@	hs@	�@��@Ĝ@�u@�@r�@Q�@A�@  @|�@\)@K�@+@�@��@��@��@ff@E�@$�@{@�@��@@p�@V@��@��@��@Z@I�@9X@�@��@��@�m@�
@�
@�
@ƨ@ƨ@ƨ@ƨ@�F@�F@dZ@o@��@�\@n�@M�@-@�@J@J@��@�@�@�#@�#@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BȴBȴBȴBǮBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBɺBȴBȴB��B��B�B�B�B�B�B�B�B�B�B�B�B�5B��BɺB�?B�B��B�uB�7B~�Bk�BN�B1'B!�B�B�B �B-B7LB=qB=qB;dB<jB?}BB�BE�BK�BF�B@�B9XB,B�B+B  B��B�B�yB�sB�B�B��BǮB��B�qB�?B��B��B��B�bB�JB�%B� Bu�BiyB^5BR�BI�B49B�B
=B
��B
�sB
�HB
�/B
�#B
�B
�
B
ƨB
�jB
��B
�oB
}�B
k�B
S�B
@�B
#�B
B	��B	�B	�sB	�B	��B	ȴB	�wB	��B	�hB	�\B	�JB	�PB	��B	��B	��B	��B	��B	�oB	�B	w�B	q�B	jB	jB	hsB	cTB	W
B	K�B	F�B	=qB	5?B	�B	�B	hB	  B��B��B��B��B�B�yB��B	B	B	B	B��B��B�B�B�HB�B�B��B�}B�RB�?B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�B~�Bx�Bt�Bu�Br�Bl�BgmBgmBgmBe`Be`BffBiyBn�Bu�Bu�Bq�Bs�Bq�Bp�Bo�Bm�Bl�BffBdZBbNB_;B[#B^5B]/B]/BZBXBT�BP�BM�BL�BJ�BH�BE�B@�B9XB9XB8RB8RB6FB33B1'B1'B-B'�B&�B$�B$�B#�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B!�B�B�B�B�B�B�B�B�B�B#�B(�B'�B'�B)�B,B,B+B)�B'�B#�B#�B%�B)�B)�B+B,B,B.B/B2-B2-B49B5?B;dB>wB?}BA�BC�BC�BD�BF�BH�BJ�BL�BO�BT�BVBXBYB]/B]/B_;B`BBdZBhsBhsBjBm�Bo�Br�Bw�B{�B{�B|�B�B�B�%B�%B�%B�%B�+B�+B�DB�JB�VB�\B�uB��B��B��B��B��B��B��B��B��B�B�-B�?B�LB�XB�dB�wB�}BBĜBƨBǮBǮBɺB��B��B��B��B��B�B�B�B�#B�/B�HB�fB�sB�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	+B	DB	bB	uB	�B	�B	�B	�B	�B	�B	!�B	"�B	'�B	+B	+B	,B	+B	,B	/B	0!B	33B	7LB	:^B	;dB	=qB	>wB	?}B	A�B	D�B	D�B	E�B	H�B	K�B	L�B	N�B	T�B	W
B	XB	\)B	^5B	bNB	hsB	jB	k�B	m�B	n�B	n�B	q�B	r�B	u�B	y�B	z�B	� B	�B	�B	�%B	�%B	�1B	�7B	�7B	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�)B	�)B	�/B	�5B	�BB	�NB	�TB	�ZB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B

=B

=B
DB
PB
VB
\B
\B
bB
bB
hB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
0!B
2-B
2-B
33B
33B
33B
49B
49B
49B
5?B
5?B
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
8RB
8RB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
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
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
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
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
[#B
[#B
[#B
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
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
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
k�B
k�B
k�B
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
q�B
r�B
q�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BȴBȴBȴBǮBȴBȴBȴBȴBȴB��BȴBȴBȴBȴBȴBȴBȴBȴBȴBȴB��BȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴBȴB��B��B��B��BɆB��B��BևB��B�qB�B�OB��B��B�;B�oB�[B��B�AB�B�7B�PB��B�oB�XB�gB��B�ABp�BRoB49B#�B�BWB!bB-�B8B>(B>B;�B<�B?�BB�BF�BL�BG�BBB;B.B�B�B �B�.B�hB��B�BٚBخB�[B��BªB�.B�LB��B�-B��B�B�PB�_B��Bw�BkB_�BT�BL�B7fB]B�BoB
�B
��B
��B
�CB
�CB
�B
ȀB
�wB
�B
�{B
�OB
nIB
W$B
C�B
&B
%B	�BB	��B	�B	��B	�vB	�^B	��B	�*B	�TB	�bB	�B	�"B	�'B	��B	��B	�qB	�xB	��B	��B	yrB	r�B	k�B	k�B	j�B	ezB	X�B	MB	H�B	@4B	9>B	!-B	YB	,B	UB��B��B��B��B�hB�B�DB	�B	[B	�B	B	 �B��B��B�wB�B�7B��B��B��B�rB�zB�GB��B��B��B�"B��B�DB��B��B�jB��B��B�$B�qB�:B�qB�IB�7B��B��B��B��B��B��By�Bu�Bv�BtTBmCBg�Bg�Bg�Be�Be�Bf�Bi�BoiBw2Bv�Br�Bt9BrGBq[Bp�Bo BnBg8Be�BcnB_�B[�B_!B^B^jB\BZQBVmBRTBO\BNVBL0BJXBG�BA�B:*B:B9$B9$B7fB4nB2�B3hB-�B(�B'�B%�B%�B$ZB"�B# B!-BpBdBBWB�B�BBEB?B�B�BBmBsBB�BsB�B=B7BkB�B�ByBmB�B�BB�B)B�B�B�BVB �B �B!�B �B"�B 'B!B�B �B;BBB�B�B$&B)�B(�B(�B*B,�B,�B,B+�B)_B$�B%B'B*�B*B+kB,qB,�B.�B/�B2�B2�B4�B6�B<PB?B@BB'BC�BDBESBG�BI7BKxBMBPHBUgBV�BX�BY�B]dB]�B_�B`�Bd�Bh�Bh�BkBnBp!Bs�BxlB|6B|6B}VB��B�MB�YB��B�YB�YB��B��B��B��B��B��B��B�B�B�B�B�B�'B�bB�ZB�sB�]B�aB�ZB��B��B��B��B��BªB��B��B��B��B��B��B��B�.B�B�TB�mB�EB�eBیBݲB��B��B�*B�)B��B�B��B��B�B�B�B�B�B�B�B�B	 B	 B	 B	AB	GB	MB	tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!�B	#:B	(>B	+B	+B	,=B	+B	,WB	/OB	0oB	3�B	7�B	:xB	;�B	=�B	>�B	?�B	A�B	D�B	D�B	E�B	H�B	K�B	MB	O(B	U2B	W?B	X_B	\]B	^�B	b�B	h�B	j�B	k�B	m�B	n�B	oB	rB	r�B	u�B	zDB	z�B	�4B	�AB	�GB	�%B	�?B	�KB	�7B	�7B	�JB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�>B	�>B	�0B	�0B	�WB	�cB	�CB	�/B	�iB	�5B	�;B	�[B	�GB	�GB	�hB	��B	�`B	�fB	�lB	�rB	��B	�dB	�jB	��B	��B	��B	��B	��B	��B	ðB	��B	ĶB	żB	��B	��B	��B	��B	��B	�B	��B	��B	� B	�B	�2B	�B	�9B	�9B	�$B	�1B	�kB	�xB	�]B	�~B	ޞB	�vB	�B	�nB	�tB	�`B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�<B	�BB
 B
 B
B
B
B
'B
AB
AB
GB
MB
mB
SB
9B
9B
?B
?B
_B
_B
�B

XB

XB
xB
jB
�B
�B
�B
�B
}B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
#B
$B
#�B
$B
#�B
$�B
$�B
$�B
$�B
%B
&B
'B
'B
'B
($B
(
B
(
B
(
B
(
B
)*B
)*B
)DB
)DB
)*B
)B
*0B
*B
+6B
,"B
-CB
-)B
-CB
-CB
.B
./B
./B
./B
.IB
/5B
/5B
/5B
/OB
0UB
0UB
0UB
1AB
0oB
2aB
2aB
3MB
3hB
3MB
4nB
4TB
4nB
5ZB
5?B
5ZB
5ZB
5ZB
5ZB
6`B
6`B
6`B
6�B
7�B
7fB
7�B
7�B
8�B
8�B
9�B
:�B
:�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
?�B
?}B
?�B
?}B
?�B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
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
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
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
L�B
L�B
MB
NB
NB
NB
N�B
OB
N�B
N�B
PB
O�B
O�B
QB
QB
R B
R B
R:B
S@B
TB
T,B
T,B
UB
T�B
T�B
T�B
UB
UB
U2B
VB
VB
V9B
VB
VSB
WYB
W$B
X+B
X+B
XEB
X+B
Y1B
YKB
YKB
ZQB
[WB
[WB
[WB
\CB
\CB
\]B
\]B
]IB
]dB
]/B
]IB
]IB
]IB
]IB
^jB
^jB
^OB
_VB
`vB
`\B
`vB
a|B
abB
a|B
a|B
bhB
bhB
cnB
cnB
c�B
c�B
d�B
dtB
dZB
e`B
ffB
ffB
f�B
f�B
f�B
f�B
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
iyB
h�B
iyB
i�B
iyB
iyB
iyB
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
j�B
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
q�B
r�B
q�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611240033482016112400334820161124003348201806221217062018062212170620180622121706201804050410042018040504100420180405041004  JA  ARFMdecpA19c                                                                20161120093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161120003532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161120003533  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161120003533  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161120003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161120003534  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161120003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161120003534  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161120003534  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161120003535                      G�O�G�O�G�O�                JA  ARUP                                                                        20161120013142                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161120153530  CV  JULD            G�O�G�O�F�ۤ                JM  ARCAJMQC2.0                                                                 20161123153348  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161123153348  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191004  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                