CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-28T00:35:10Z creation;2018-07-28T00:35:15Z conversion to V3.1;2019-12-19T07:36:29Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180728003510  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_264                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�u3q5y�1   @�u46��@9�Q���dX�w�kQ1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB��B��B��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� DfD�fD  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D���D�@ Dր D�� D�  D�@ D׀ D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�B (�B�\B�\B\)B\)B'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�
C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)Du�D�)D|)D�D��D�)D|)D�)D|)D�)D|)D�)D|)D��D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-��D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DU�DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AHD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD���D�>D�~D־D��D�>D�~D׾D���D�:�D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�:�D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�AHD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aχ+AσAσAσAσAυAσAσAυA�|�A�\)A�-A΃A�S�A��/AɍPA�v�A���A�dZA�C�A�%A�\)A��RA�|�A��A���A��HA��mA��+A�p�A�5?A���A��#A���A���A���A�jA��/A��PA���A��/A��-A�oA�C�A�oA��`A�x�A�-A�hsA���A��jA�I�A���A���A���A�$�A��hA�ZA�  A�\)A�%A�/A�dZA�%A��\A���A���A��;A���A���A���A���A�7LA���A�v�A�E�A�ĜA�S�A��/A��PA�r�A��A�p�A�?}A~bNA}oA{p�AzZAw��AvAu�-At~�AsG�Aq�^Ap�yApv�ApAo&�Am�Al=qAj��Aj�\AjJAi��Ah��Ag��Ag7LAf�/Af��Af�Af�DAep�Ac��Ab�AaVA_��A^v�A]�;A\$�AY"�AX �AW�PAV��AU�wAUoATjATAS&�AQ�APn�AOhsAN�/AM�AKAI�;AH(�AGAF�DAFZAFJAEAEp�AE;dADz�ADA�AC�AC\)AA�#AA&�A@��A?A>��A>�+A>{A<��A;C�A:A�A9G�A8I�A5t�A3�-A2�A1�A1XA0�HA0�A/�A.v�A-�A-�-A-/A-A,�!A,VA,9XA,�A+�7A*�!A*9XA*{A)��A)ƨA)�A(E�A'��A&��A&�jA&�\A&{A%�FA$z�A#�;A#O�A"^5A!O�A -A�wA�PA��AbA\)A��A�
AȴA �A��A"�A��A{A�AA  A%A�+A�wA�
A�\A��A�hA�yA�A�AffA��A
�RA
5?A
�A	�
A	C�A	A��AJAZA�A�A\)A�A�AZA�A"�@���@��^@�Q�@���@�7L@�@�ƨ@�$�@�@�Z@�1'@�\@�hs@�bN@睲@�-@��H@�&�@�Q�@�K�@�J@�G�@���@ڏ\@�dZ@�V@�A�@Ӿw@���@��@�5?@�j@���@�1'@�33@�"�@�?}@�Ĝ@î@�;d@�&�@�+@�J@��#@���@��7@�x�@��/@��m@�ƨ@���@�|�@�t�@�33@��@�^5@���@��`@�z�@�(�@���@�n�@���@�?}@��u@�  @�K�@��+@�V@��@���@�ff@���@�=q@��T@�1'@�33@��+@��#@��@��9@��@�9X@��m@���@��H@�E�@��@�O�@��`@��@�9X@�b@�\)@���@�5?@�p�@��@��@��m@�ƨ@�l�@�;d@��@��@��R@��\@�v�@�^5@�5?@�@�&�@�r�@��m@��F@���@�;d@��#@�`B@�/@���@�  @��@�C�@�+@���@��H@��H@��H@��@��!@���@�G�@�V@��j@�  @��@�|�@�l�@��@�-@��T@�?}@�Ĝ@��@�1'@��m@�\)@���@�V@���@��h@�?}@���@�Z@�b@�ƨ@�~�@�@���@��j@��@�A�@��w@�o@��H@���@�~�@�-@���@�hs@���@��D@�A�@�1@��@��@��;@��F@�\)@��@���@��y@���@�E�@���@���@��h@�X@�G�@�%@��/@��j@���@��@�r�@�bN@�(�@l�@\)@K�@;d@~E�@}`B@}O�@}/@}/@}�@|�@|j@{��@{�@{33@z��@z~�@zM�@z-@y��@y�@y�#@y��@y7L@x��@x�u@xr�@xQ�@x �@x  @w�;@w��@w�w@w�P@wK�@w;d@w�@v�@v�R@v�+@vv�@vv�@vV@u�-@t��@t�@t�j@t1@s�m@s�
@s��@sdZ@r��@r��@r=q@q��@qx�@q&�@p�`@p�9@o�w@n��@nȴ@n�@n�y@n�y@n��@nff@nff@n5?@m��@m`B@m�@l�j@lz�@lZ@lI�@k��@k33@j�H@j��@jn�@jM�@j-@i��@iG�@i�@h�u@hQ�@hQ�@hQ�@h1'@h  @g�w@g
=@f��@fE�@f@e�T@e�h@e�@d�j@d�@c��@b�@b~�@a��@aG�@a%@`��@`�@`Q�@`1'@`b@`b@`  @_�w@_|�@_
=@^�R@^V@]�T@]��@]�-@]�@\��@\9X@[33@Z��@Zn�@Z^5@Y�#@YG�@Y&�@Y�@X�u@Xb@W�@W�w@W;d@W
=@V��@Vv�@VV@V5?@U�@U@U�@U�@TZ@S��@Sƨ@S��@S33@S@S@R�@R�H@R=q@Q��@PĜ@P�@P1'@O��@O��@O|�@O\)@O;d@O+@O+@N��@Nȴ@NV@M@M�@M/@MV@L��@Lj@L�@K�
@K�@K"�@J�@J-@Ihs@IG�@H��@H1'@H �@Hb@G�w@G�@G�@G|�@G\)@G+@G
=@F��@F�y@F��@Fff@Fff@F{@E�@E?}@E/@D�/@D1@C�
@C�
@Cƨ@C��@CdZ@B�@B�\@B=q@B�@A�@AG�@@Ĝ@@ �@?�w@?|�@?+@>�y@>5?@>@=��@=`B@=/@<�j@<9X@;�
@;�F@;33@:�!@:M�@9��@9��@9��@9x�@9G�@9�@8��@8��@8�u@8r�@81'@8  @7��@7��@7\)@7+@6�R@6v�@6$�@6@5�@5@5`B@4�@4�@4z�@49X@41@3�m@3�
@3��@3t�@3S�@3C�@3"�@2��@2�\@2n�@2�@1�^@1�7@1G�@17L@1�@1%@0��@0��@0bN@/�;@/+@.�@.��@.V@-��@-�-@-��@-�@-p�@-`B@-O�@-/@-V@,�@,�@,(�@+�m@+��@+dZ@+@*�!@*n�@*M�@)�^@)&�@(Ĝ@(bN@(A�@( �@'��@'\)@'+@&�@&��@&v�@&ff@&V@&E�@&5?@&{@&@%�-@%�@%`B@%?}@$��@$��@$9X@$(�@$�@$1@#�m@#ƨ@#�F@#t�@#C�@#S�@#S�@#C�@#"�@#o@#@"�H@"~�@"�@"J@!��@!�@!�#@!��@!��@!��@!hs@!�@ �`@ �9@ ��@ �@ Q�@ Q�@ A�@ b@|�@
=@ȴ@�R@�+@{@�@@p�@`B@O�@?}@?}@�@��@Z@�m@ƨ@ƨ@�F@C�@��@��@M�@J@�#@��@�^@�^@��@hs@G�@�@��@��@��@��@�`@��@��@�@bN@A�@1'@  @�@�;@�;@��@�w@��@\)@+@�@��@�y@ȴ@��@��@��@�+@ff@{@@�-@��@�@�@�@��@I�@ƨ@��@t�@dZ@33@�!@~�@^5@J@��@hs@X@hs@G�@%@��@r�@Q�@ �@  @�@�@l�@K�@+@
=@��@�@��@v�@E�@$�@��@O�@/@�@�/@�j@�D@9X@�
@�@33@"�@@
�@
��@
��@
�\@
n�@
n�@
n�@
^5@
-@
�@	��@	�^@	7L@�9@r�@Q�@1'@ �@  @�@�;@|�@;d@+@�@��@�y@�y@�@��@ff@@��@�h@?}@V@�/@�@z�@j@(�@�@1@��@ƨ@��@��@��@�@t�@dZ@S�@33@o@�@�\@n�@M�@�@�#@�^@�7@x�@�7@��@hs@7L@�@ r�@  �@ b@   ?��?�V?��?���?��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aχ+AσAσAσAσAυAσAσAυA�|�A�\)A�-A΃A�S�A��/AɍPA�v�A���A�dZA�C�A�%A�\)A��RA�|�A��A���A��HA��mA��+A�p�A�5?A���A��#A���A���A���A�jA��/A��PA���A��/A��-A�oA�C�A�oA��`A�x�A�-A�hsA���A��jA�I�A���A���A���A�$�A��hA�ZA�  A�\)A�%A�/A�dZA�%A��\A���A���A��;A���A���A���A���A�7LA���A�v�A�E�A�ĜA�S�A��/A��PA�r�A��A�p�A�?}A~bNA}oA{p�AzZAw��AvAu�-At~�AsG�Aq�^Ap�yApv�ApAo&�Am�Al=qAj��Aj�\AjJAi��Ah��Ag��Ag7LAf�/Af��Af�Af�DAep�Ac��Ab�AaVA_��A^v�A]�;A\$�AY"�AX �AW�PAV��AU�wAUoATjATAS&�AQ�APn�AOhsAN�/AM�AKAI�;AH(�AGAF�DAFZAFJAEAEp�AE;dADz�ADA�AC�AC\)AA�#AA&�A@��A?A>��A>�+A>{A<��A;C�A:A�A9G�A8I�A5t�A3�-A2�A1�A1XA0�HA0�A/�A.v�A-�A-�-A-/A-A,�!A,VA,9XA,�A+�7A*�!A*9XA*{A)��A)ƨA)�A(E�A'��A&��A&�jA&�\A&{A%�FA$z�A#�;A#O�A"^5A!O�A -A�wA�PA��AbA\)A��A�
AȴA �A��A"�A��A{A�AA  A%A�+A�wA�
A�\A��A�hA�yA�A�AffA��A
�RA
5?A
�A	�
A	C�A	A��AJAZA�A�A\)A�A�AZA�A"�@���@��^@�Q�@���@�7L@�@�ƨ@�$�@�@�Z@�1'@�\@�hs@�bN@睲@�-@��H@�&�@�Q�@�K�@�J@�G�@���@ڏ\@�dZ@�V@�A�@Ӿw@���@��@�5?@�j@���@�1'@�33@�"�@�?}@�Ĝ@î@�;d@�&�@�+@�J@��#@���@��7@�x�@��/@��m@�ƨ@���@�|�@�t�@�33@��@�^5@���@��`@�z�@�(�@���@�n�@���@�?}@��u@�  @�K�@��+@�V@��@���@�ff@���@�=q@��T@�1'@�33@��+@��#@��@��9@��@�9X@��m@���@��H@�E�@��@�O�@��`@��@�9X@�b@�\)@���@�5?@�p�@��@��@��m@�ƨ@�l�@�;d@��@��@��R@��\@�v�@�^5@�5?@�@�&�@�r�@��m@��F@���@�;d@��#@�`B@�/@���@�  @��@�C�@�+@���@��H@��H@��H@��@��!@���@�G�@�V@��j@�  @��@�|�@�l�@��@�-@��T@�?}@�Ĝ@��@�1'@��m@�\)@���@�V@���@��h@�?}@���@�Z@�b@�ƨ@�~�@�@���@��j@��@�A�@��w@�o@��H@���@�~�@�-@���@�hs@���@��D@�A�@�1@��@��@��;@��F@�\)@��@���@��y@���@�E�@���@���@��h@�X@�G�@�%@��/@��j@���@��@�r�@�bN@�(�@l�@\)@K�@;d@~E�@}`B@}O�@}/@}/@}�@|�@|j@{��@{�@{33@z��@z~�@zM�@z-@y��@y�@y�#@y��@y7L@x��@x�u@xr�@xQ�@x �@x  @w�;@w��@w�w@w�P@wK�@w;d@w�@v�@v�R@v�+@vv�@vv�@vV@u�-@t��@t�@t�j@t1@s�m@s�
@s��@sdZ@r��@r��@r=q@q��@qx�@q&�@p�`@p�9@o�w@n��@nȴ@n�@n�y@n�y@n��@nff@nff@n5?@m��@m`B@m�@l�j@lz�@lZ@lI�@k��@k33@j�H@j��@jn�@jM�@j-@i��@iG�@i�@h�u@hQ�@hQ�@hQ�@h1'@h  @g�w@g
=@f��@fE�@f@e�T@e�h@e�@d�j@d�@c��@b�@b~�@a��@aG�@a%@`��@`�@`Q�@`1'@`b@`b@`  @_�w@_|�@_
=@^�R@^V@]�T@]��@]�-@]�@\��@\9X@[33@Z��@Zn�@Z^5@Y�#@YG�@Y&�@Y�@X�u@Xb@W�@W�w@W;d@W
=@V��@Vv�@VV@V5?@U�@U@U�@U�@TZ@S��@Sƨ@S��@S33@S@S@R�@R�H@R=q@Q��@PĜ@P�@P1'@O��@O��@O|�@O\)@O;d@O+@O+@N��@Nȴ@NV@M@M�@M/@MV@L��@Lj@L�@K�
@K�@K"�@J�@J-@Ihs@IG�@H��@H1'@H �@Hb@G�w@G�@G�@G|�@G\)@G+@G
=@F��@F�y@F��@Fff@Fff@F{@E�@E?}@E/@D�/@D1@C�
@C�
@Cƨ@C��@CdZ@B�@B�\@B=q@B�@A�@AG�@@Ĝ@@ �@?�w@?|�@?+@>�y@>5?@>@=��@=`B@=/@<�j@<9X@;�
@;�F@;33@:�!@:M�@9��@9��@9��@9x�@9G�@9�@8��@8��@8�u@8r�@81'@8  @7��@7��@7\)@7+@6�R@6v�@6$�@6@5�@5@5`B@4�@4�@4z�@49X@41@3�m@3�
@3��@3t�@3S�@3C�@3"�@2��@2�\@2n�@2�@1�^@1�7@1G�@17L@1�@1%@0��@0��@0bN@/�;@/+@.�@.��@.V@-��@-�-@-��@-�@-p�@-`B@-O�@-/@-V@,�@,�@,(�@+�m@+��@+dZ@+@*�!@*n�@*M�@)�^@)&�@(Ĝ@(bN@(A�@( �@'��@'\)@'+@&�@&��@&v�@&ff@&V@&E�@&5?@&{@&@%�-@%�@%`B@%?}@$��@$��@$9X@$(�@$�@$1@#�m@#ƨ@#�F@#t�@#C�@#S�@#S�@#C�@#"�@#o@#@"�H@"~�@"�@"J@!��@!�@!�#@!��@!��@!��@!hs@!�@ �`@ �9@ ��@ �@ Q�@ Q�@ A�@ b@|�@
=@ȴ@�R@�+@{@�@@p�@`B@O�@?}@?}@�@��@Z@�m@ƨ@ƨ@�F@C�@��@��@M�@J@�#@��@�^@�^@��@hs@G�@�@��@��@��@��@�`@��@��@�@bN@A�@1'@  @�@�;@�;@��@�w@��@\)@+@�@��@�y@ȴ@��@��@��@�+@ff@{@@�-@��@�@�@�@��@I�@ƨ@��@t�@dZ@33@�!@~�@^5@J@��@hs@X@hs@G�@%@��@r�@Q�@ �@  @�@�@l�@K�@+@
=@��@�@��@v�@E�@$�@��@O�@/@�@�/@�j@�D@9X@�
@�@33@"�@@
�@
��@
��@
�\@
n�@
n�@
n�@
^5@
-@
�@	��@	�^@	7L@�9@r�@Q�@1'@ �@  @�@�;@|�@;d@+@�@��@�y@�y@�@��@ff@@��@�h@?}@V@�/@�@z�@j@(�@�@1@��@ƨ@��@��@��@�@t�@dZ@S�@33@o@�@�\@n�@M�@�@�#@�^@�7@x�@�7@��@hs@7L@�@ r�@  �@ b@   ?��?�V?��?���?��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��BǮB��B�B1B;dBp�B��B|�B�{B�BB�dB��B��B�RB�B�3B��B��B�=B�B}�B�B� B�Bw�BgmBN�BF�BB�B?}B6FB#�B�BB��B�B�B�B�HB��BǮB�jB�?B�B�!B�B��B��B�oB�B{�Bo�B^5BI�B>wB�B�B	7B	7BB
��B
��B
�B
�mB
�5B
�B
��B
�LB
��B
~�B
r�B
dZB
[#B
N�B
D�B
0!B
&�B
+B
�B
�B
{B
�B
�B
�B
\B
B	��B	�B	��B	�B	�B	�mB	�/B	�HB	�5B	�5B	�#B	��B	ɺB	�RB	�-B	��B	��B	��B	��B	�\B	y�B	~�B	�B	z�B	s�B	p�B	l�B	hsB	_;B	P�B	M�B	I�B	E�B	<jB	0!B	+B	�B	!�B	#�B	#�B	!�B	�B	�B	�B	�B	�B	hB	DB	B��B	  B��B�B�B�B�ZB�B�B�B��B�dB�jB�LB�jB�XB�RB�-B�B�B�B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�{B��B��B�oB�\B�+B�%B�%B� B{�By�B}�B}�Bx�Br�Br�Bm�Bk�BgmBiyBiyBiyBe`BdZBbNB_;BVBS�BVBO�BE�BK�BP�BO�BK�BD�B?}BC�BA�B<jB?}BG�BE�BA�BC�B?}B7LB(�B,B-B1'B49B1'B33B.B&�B"�B%�B&�B&�B�B{B�B�B�B�B%BuB�B�B�BuBJB{B�B�B�B�B�BhBJB�B{B�B�BoB	7BhB�B�B �B)�B&�B2-B.B1'B)�B&�B1'B6FB8RB8RB8RB7LB8RBA�BA�BA�BA�B@�B>wB>wB>wB>wBA�BA�BA�B?}BA�BE�BC�BC�BB�B@�B>wBB�BI�BE�BL�BQ�BR�BN�BQ�BT�BVBXBXB]/B]/B^5B_;B]/B`BBdZBcTBffBiyBjBk�BjBjBm�Bq�Bt�B|�B� B�B� B�B�B�B�B�B�%B�%B�%B�B�B�%B�DB�\B�bB�VB�DB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�9B�-B�3B�dB�^B�wBĜBŢBȴBȴB��B��B��B��B��B�B�)B�;B�HB�5B�sB�B��B��B��B��B��B	B	%B	B	%B	1B	
=B	PB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	$�B	'�B	)�B	+B	.B	.B	2-B	49B	6FB	8RB	9XB	9XB	8RB	9XB	A�B	A�B	A�B	@�B	E�B	M�B	N�B	O�B	O�B	O�B	R�B	S�B	YB	ZB	[#B	]/B	_;B	`BB	`BB	bNB	dZB	cTB	dZB	gmB	hsB	jB	jB	k�B	l�B	m�B	m�B	n�B	n�B	o�B	p�B	p�B	q�B	r�B	r�B	t�B	t�B	s�B	s�B	v�B	|�B	}�B	~�B	�B	�B	�B	�%B	�B	�7B	�=B	�JB	�PB	�VB	�\B	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�-B	�?B	�RB	�RB	�^B	�jB	�jB	�jB	�jB	�jB	�jB	�wB	��B	B	ÖB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�/B	�/B	�5B	�BB	�BB	�;B	�5B	�;B	�BB	�ZB	�fB	�mB	�fB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
%B
+B

=B
	7B

=B
PB
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
\B
\B
bB
\B
\B
hB
hB
hB
hB
{B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
!�B
"�B
#�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
(�B
'�B
(�B
)�B
+B
+B
+B
)�B
+B
,B
,B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
33B
49B
33B
49B
6FB
7LB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
6FB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
9XB
9XB
;dB
;dB
=qB
=qB
<jB
=qB
>wB
>wB
?}B
@�B
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
@�B
A�B
A�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
G�B
F�B
H�B
I�B
J�B
J�B
I�B
K�B
K�B
K�B
M�B
L�B
L�B
L�B
L�B
K�B
K�B
L�B
N�B
O�B
N�B
M�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
R�B
R�B
R�B
S�B
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
W
B
VB
VB
W
B
XB
XB
W
B
W
B
XB
XB
XB
XB
ZB
ZB
ZB
ZB
YB
[#B
\)B
[#B
[#B
]/B
^5B
^5B
]/B
]/B
]/B
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
`BB
aHB
aHB
aHB
aHB
bNB
cTB
dZB
cTB
dZB
dZB
cTB
dZB
dZB
e`B
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
gmB
ffB
e`B
ffB
hsB
iyB
iyB
jB
jB
jB
jB
iyB
iyB
jB
k�B
k�B
k�B
l�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
q�B
r�B
q�B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
u�B
u�B
t�B
s�B
s�B
q�B
r�B
t�B
t�B
s�B
s�B
t�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B�B�PBοBɠB�B��BBF�BzxB��B��B��B�/B�3B��BªBB�*B�AB�nB��B��B�B��B��B�B��B�-By�Bj�BR�BJ�BEBA B8lB&�BxBmB��B�UB�B�6B�BּB��B�(B��B�UB��B�B�TB��B�FB��B|�BqB`vBK�BAUBQB?BB
	BB
��B
�FB
�vB
�B
�VB
�?B
�0B
�DB
�IB
��B
utB
f�B
\�B
P�B
FtB
33B
(�B
+�B
 \B
=B
9B
�B
IB
_B
�B
�B	��B	�?B	�8B	�nB	�IB	��B	ބB	��B	޸B	ބB	�WB	՛B	�^B	��B	�B	��B	��B	�!B	��B	��B	}<B	�OB	��B	|B	t�B	q�B	mwB	iDB	`�B	SB	OBB	KB	F�B	>�B	2-B	-]B	 �B	#B	$ZB	$&B	"4B	;B	5B	#B	mB	B	B	JB	�B	  B	 �B�$B��B�hB�B��B�=B�sBרBҽB��B�wB��B�VB�DB�$B�MB�IB��B��B��B��B��B��B��B�WB�B��B��B�nB�LB�,B�NB��B��B�eB�gB��B��B�&B�.B��B�B�+B�oB}VB{JB~�B~�By�Bs�Bs�Bn�Bl�Bh�BjBjKBjeBfLBe,Bc:B`\BW�BUMBV�BQNBHBMjBQ�BP�BL�BFB@�BDgBBuB=�B@OBG�BF?BB[BDB@iB8�B+QB-�B.�B1�B4�B2B3�B.�B(XB$�B'8B(
B'�BdB�B�B�BdB�B�B{BKB_BkB�B"B�BQBKB_BQBeBB"BsB�BIB�BB)B�B�B?B!�B*KB($B2�B/ B1�B+�B(XB1�B6�B8�B8�B8�B8B9	BA�BA�BA�BA�B@�B>�B>�B?.B?BBBBBB'B@OBB'BF%BDBD3BC-BAUB?}BCGBJ#BF�BM6BQ�BSuBP.BR�BU�BV�BXyBX�B]dB]~B^�B_�B]�B`�Bd�Bc�Bf�Bi�Bj�Bk�BkBkBnBrGBu�B}<B�OB�;B�iB�UB�[B�aB�aB�SB�?B�YB�YB�mB��B��B��B��B��B��B�0B��B��B�B�1B�)B��B��B�B�B��B��B�B�4B�bB�tB�KB�eB��B�oB�hB�nB��B��B��B��B��B��B��B�B�7B�DB�.B�:B�[B�MBևBܒBߤB�B�!B��B�6B��B�$B�*B�dB�qB	MB	YB	mB	�B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!B	"4B	%B	($B	*0B	+QB	./B	.IB	2GB	4nB	6`B	8lB	9rB	9rB	8�B	9�B	A�B	A�B	A�B	@�B	E�B	M�B	N�B	O�B	O�B	PB	S@B	TFB	YKB	ZQB	[WB	]dB	_VB	`vB	`vB	bhB	d�B	c�B	d�B	g�B	h�B	j�B	j�B	k�B	l�B	m�B	m�B	n�B	n�B	o�B	p�B	p�B	q�B	r�B	r�B	t�B	t�B	s�B	tB	w2B	}B	~B	HB	�B	�9B	�9B	�YB	��B	�lB	�rB	�~B	��B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�>B	�_B	�)B	�/B	�;B	�AB	�GB	�aB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	�	B	��B	�B	�B	�4B	�:B	�B	�B	�$B	�KB	�7B	�WB	�#B	�=B	�WB	�=B	�WB	�IB	�dB	ބB	�\B	�\B	�pB	ބB	ߊB	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�8B	�B	�B	�B	�B	�B
 4B
 4B
  B
  B
 4B
 4B	�.B
 OB
-B
-B
3B
3B
gB
SB
9B
SB
YB
EB
tB
�B

rB
	lB

rB
PB
jB
�B
VB
\B
vB
vB
�B
}B
}B
�B
vB
�B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
 �B
!�B
!B
!�B
#B
$B
%�B
&B
%�B
%�B
%�B
'B
%�B
'�B
(
B
'B
(
B
(
B
(
B
(
B
)B
(>B
)B
*0B
+6B
+B
+B
*0B
+6B
,"B
,=B
-)B
-)B
.IB
.IB
./B
.IB
/OB
/5B
/5B
/OB
/5B
0;B
0UB
0UB
1AB
1[B
2-B
2aB
2GB
1AB
1AB
1AB
1[B
1�B
3MB
4TB
3hB
4�B
6zB
7fB
6`B
7LB
7fB
7fB
7�B
7�B
7fB
7fB
6zB
7fB
8�B
8lB
8�B
9rB
9rB
:xB
9�B
9�B
;�B
;�B
=�B
=�B
<�B
=�B
>�B
>�B
?�B
@�B
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
@�B
A�B
A�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
G�B
F�B
H�B
I�B
J�B
J�B
I�B
K�B
K�B
K�B
M�B
L�B
L�B
L�B
L�B
LB
K�B
MB
N�B
O�B
N�B
N"B
OB
O�B
O�B
Q B
R B
RB
Q�B
RB
R B
RB
R B
SB
S&B
S�B
TB
S�B
S�B
SB
S&B
SB
T,B
T,B
TB
T,B
T�B
T�B
T�B
T�B
UB
UB
U2B
UB
VB
V9B
VB
VB
V9B
W$B
W$B
W
B
VB
VB
W$B
X+B
XB
W$B
W?B
X+B
X+B
XEB
XEB
Z7B
Z7B
Z7B
Z7B
YKB
[=B
\CB
[WB
[WB
]dB
^5B
^5B
]IB
]dB
]IB
^OB
^OB
_VB
_VB
_pB
_VB
_VB
`\B
`vB
`\B
aHB
a|B
`\B
a|B
abB
abB
a|B
b�B
cnB
dZB
cnB
dtB
d�B
c�B
dtB
dtB
e�B
g�B
g�B
g�B
g�B
g�B
hsB
hsB
hsB
hsB
h�B
h�B
h�B
g�B
f�B
e�B
f�B
h�B
i�B
i�B
jB
j�B
jB
j�B
i�B
i�B
jB
k�B
k�B
k�B
l�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
q�B
r�B
q�B
s�B
s�B
r�B
r�B
s�B
s�B
s�B
u�B
u�B
t�B
s�B
s�B
q�B
r�B
t�B
t�B
s�B
s�B
t�B
u�B
v�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808010037072018080100370720180801003707201808010200162018080102001620180801020016201808020027262018080200272620180802002726  JA  ARFMdecpA19c                                                                20180728093509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180728003510  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180728003513  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180728003513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180728003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180728003514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180728003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180728003514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180728003514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180728003515                      G�O�G�O�G�O�                JA  ARUP                                                                        20180728005532                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180728153829  CV  JULD            G�O�G�O�Fé�                JM  ARSQJMQC2.0                                                                 20180730000000  CF  PSAL_ADJUSTED_QCD���D���G�O�                JM  ARCAJMQC2.0                                                                 20180731153707  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180731153707  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180731170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180801152726  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                