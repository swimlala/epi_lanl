CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-08T00:35:24Z creation;2018-01-08T00:35:27Z conversion to V3.1;2019-12-19T07:48:12Z update;     
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
resolution        =���   axis      Z        P  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  s   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180108003524  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_198                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�B���k 1   @�B���O�@4Aa��e��dp�6z�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ Dμ�D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�RA��BBBBB'B/B7\)B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*��D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@u�D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~Dκ�D��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�ffA�ffA�jA�hsA�jA�hsA�l�A�n�A�n�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�l�A�l�A�jA�hsA�ffA�^5A�VA�=qA�/A�/A��A�A�A�1A�VA�{A��A�/A�dZA��A�"�A�9XAƝ�A�M�A�l�A�oAčPA�{A�XA���A�M�A��A�{A�-A��\A��A�-A���A���A�\)A���A��!A��;A��9A��A�jA��wA�n�A�p�A��TA�~�A�\)A��-A���A��+A�z�A�l�A�{A���A�9XA��7A�5?A�$�A�-A��-A���A�^5A�$�A���A���A��A���A�^5A�(�A�ZA�"�A���A�=qA�v�A���A�G�A�ĜA��#A���A���A��A���A�+A��DA�=qA�  A���A�"�A�&�A�K�A�{A��#A���A�7LA��\A�%A}O�A{\)Az(�Ay�Ax$�Av�AshsAr1Ap~�Amt�Aj�DAi�Ai�7AiAh(�AfA�AeO�A`��A]`BA[�7AZ��AZ��AZ�AY�AX �AV��AU�#AU|�AUG�AS��AQXAQC�AQAP�9APz�AO/AL��AK;dAI`BAGAEC�AD�uAC7LAAG�A?�A<�yA;�wA;
=A:I�A9hsA9�A8�A8I�A6�A2^5A1t�A0��A/&�A.��A.1A,�/A*�!A*$�A)�A)hsA'��A&E�A%�mA$r�A#33A!�TA!?}A?}A^5A��A��A%A-A��A��AbNA��A�+A�AdZA�A��At�AffA�mA��AhsA
�A
z�A	`BA��A�jAr�A1'A��A;dA��A(�A�A �A�AK�A7LA Ĝ@��
@�9X@�\)@�"�@��H@�n�@��T@��h@���@���@���@�;d@��@�V@�|�@�  @�V@�J@�X@�u@�  @�t�@��H@⟾@�/@�5?@�G�@ܛ�@�I�@�;d@؋D@�
=@���@�E�@Չ7@��@�%@�Z@���@�~�@�x�@��;@�"�@�~�@͙�@�V@̓u@��@�"�@���@ɡ�@�x�@���@�t�@ƸR@�5?@ŉ7@���@�r�@î@��y@�~�@�@��@�r�@��@��@�ff@���@��-@�/@��`@��@�1'@�+@��+@�M�@�$�@��@�O�@�r�@���@�33@�@��@�Ĝ@�Q�@��
@���@�t�@�;d@���@��R@��+@�E�@��#@�`B@�Ĝ@��w@�S�@��@�-@��7@�hs@�V@�Z@�33@�M�@�J@��@��@��@���@��@�j@�j@��@��`@��/@��j@�bN@���@��P@�l�@�
=@��@�\)@��R@�@�{@��@��@��7@��@�z�@�r�@�  @���@�+@��!@�^5@�~�@��+@���@��@�
=@��!@�^5@���@�z�@�z�@�1'@�+@�@���@�ff@��@���@��T@���@��h@�p�@�O�@�?}@��/@��u@��u@�r�@�1'@���@��w@��@��@�t�@�S�@�33@���@��R@��\@���@��!@��!@���@���@��\@�ff@�V@�-@�{@��@�O�@���@���@��j@��@�Z@�9X@�b@��;@�ƨ@��@�|�@�t�@�dZ@�33@���@��@��@��@�ȴ@�~�@�^5@��@��-@�X@�7L@��@��`@�r�@��@��
@���@��@�l�@�K�@�+@��@��\@�{@���@��h@��j@���@��D@�Z@�  @���@��w@�l�@�
=@��R@�-@��@���@�V@��u@��@�z�@�j@�j@�Z@�9X@� �@��m@���@�;d@��@��y@���@��+@�5?@��-@��7@�`B@�G�@�%@��@��@��`@��/@���@��D@�9X@� �@��w@�K�@���@��@���@��+@�n�@�=q@�{@�{@�{@�@��T@��#@��-@��@�r�@�Q�@�9X@�(�@��@�1@�  @��@��
@���@�\)@�@��@��y@��@���@��!@�^5@��@��#@���@�hs@�7L@�/@��@�Z@�A�@�b@��@
=@~��@}��@}p�@}V@|��@|j@|9X@|�@{ƨ@{"�@z^5@z�@y��@y��@y�@y�#@y�7@y�@x�`@x�@w�@w�P@w|�@w;d@v��@v��@vff@u�-@t��@s��@s��@s�@sC�@r�H@r^5@q�@q7L@p�u@o�;@o�@n��@n��@nv�@n$�@m?}@l��@l�j@lI�@k�m@k��@kC�@k@j�\@jM�@j-@i��@i�@iX@i&�@h��@hQ�@g�@gl�@g;d@g�@fȴ@fE�@e�h@e�@dj@d(�@c��@c��@c�m@c�
@c�@cS�@cC�@co@b�!@b-@b-@b�@bJ@a�#@aX@a�@`�9@`bN@`A�@_�;@_�@^�y@^V@^@]�@]�T@]��@]O�@\��@\I�@[��@Z�H@Zn�@Z-@Y��@Yx�@Yx�@Y&�@Y�@X��@X��@X1'@W�w@W�P@Wl�@Vȴ@VE�@V{@U��@U�h@U`B@T�j@T��@Tj@T9X@S��@S"�@R��@R�!@RM�@Q�#@QX@P��@P�9@Pr�@P1'@O�@O;d@O
=@Nȴ@NE�@M@M�-@Mp�@M?}@L�/@L�D@Lz�@L9X@K��@K�F@J�@J�\@J^5@JM�@J�@I��@I�#@I��@I�^@Ix�@IX@I&�@H��@H��@HQ�@H �@H  @G�@G��@G�w@G|�@G;d@F�@F�+@FE�@E��@E��@E`B@Dz�@D�@C�
@C�F@C��@C��@CdZ@B��@BJ@A��@AG�@A%@@�`@@bN@?��@?+@>ff@=p�@<��@<�@<��@<��@<z�@<j@<9X@<1@;dZ@:��@:~�@:-@9�@9��@9%@8�`@8��@8r�@8A�@7�w@7K�@6�y@6ȴ@6ȴ@6E�@5�@5?}@5V@4�j@4z�@4j@4I�@4�@3��@2�\@2-@1��@1&�@0�9@0�@0bN@0A�@01'@0b@/��@/K�@.��@.��@.v�@.V@.5?@.$�@.$�@.@-��@-�-@-`B@,�@,�@,��@,Z@,(�@+�m@+�
@+ƨ@+t�@+33@+@*�H@*��@*�@)�^@)��@)hs@)&�@(��@(1'@'�w@'|�@&��@&�+@&ff@&$�@%�h@%�@%p�@%O�@%V@$�/@$�j@$z�@#��@#��@#�@#S�@#@"�H@"~�@"-@!�@!��@!�7@!�@ �@   @��@��@K�@��@��@v�@E�@�T@�@?}@/@/@V@�/@��@��@�@��@�D@z�@j@I�@9X@9X@(�@1@ƨ@�F@��@��@t�@S�@o@@�H@��@n�@�@��@x�@��@�9@r�@A�@ �@�@�w@��@�P@l�@\)@K�@�@��@��@E�@$�@{@{@�T@�-@��@��@�h@`B@?}@/@�/@��@��@�D@z�@9X@��@33@�@��@n�@=q@�@�@��@��@�7@x�@X@G�@&�@%@��@�9@bN@1'@  @��@�@�P@l�@K�@
=@�y@v�@��@�+@V@E�@E�@$�@@��@@@@��@`B@`B@?}@��@�/@�j@�@�D@z�@z�@j@I�@I�@(�@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA�ffA�ffA�jA�hsA�jA�hsA�l�A�n�A�n�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�l�A�l�A�jA�hsA�ffA�^5A�VA�=qA�/A�/A��A�A�A�1A�VA�{A��A�/A�dZA��A�"�A�9XAƝ�A�M�A�l�A�oAčPA�{A�XA���A�M�A��A�{A�-A��\A��A�-A���A���A�\)A���A��!A��;A��9A��A�jA��wA�n�A�p�A��TA�~�A�\)A��-A���A��+A�z�A�l�A�{A���A�9XA��7A�5?A�$�A�-A��-A���A�^5A�$�A���A���A��A���A�^5A�(�A�ZA�"�A���A�=qA�v�A���A�G�A�ĜA��#A���A���A��A���A�+A��DA�=qA�  A���A�"�A�&�A�K�A�{A��#A���A�7LA��\A�%A}O�A{\)Az(�Ay�Ax$�Av�AshsAr1Ap~�Amt�Aj�DAi�Ai�7AiAh(�AfA�AeO�A`��A]`BA[�7AZ��AZ��AZ�AY�AX �AV��AU�#AU|�AUG�AS��AQXAQC�AQAP�9APz�AO/AL��AK;dAI`BAGAEC�AD�uAC7LAAG�A?�A<�yA;�wA;
=A:I�A9hsA9�A8�A8I�A6�A2^5A1t�A0��A/&�A.��A.1A,�/A*�!A*$�A)�A)hsA'��A&E�A%�mA$r�A#33A!�TA!?}A?}A^5A��A��A%A-A��A��AbNA��A�+A�AdZA�A��At�AffA�mA��AhsA
�A
z�A	`BA��A�jAr�A1'A��A;dA��A(�A�A �A�AK�A7LA Ĝ@��
@�9X@�\)@�"�@��H@�n�@��T@��h@���@���@���@�;d@��@�V@�|�@�  @�V@�J@�X@�u@�  @�t�@��H@⟾@�/@�5?@�G�@ܛ�@�I�@�;d@؋D@�
=@���@�E�@Չ7@��@�%@�Z@���@�~�@�x�@��;@�"�@�~�@͙�@�V@̓u@��@�"�@���@ɡ�@�x�@���@�t�@ƸR@�5?@ŉ7@���@�r�@î@��y@�~�@�@��@�r�@��@��@�ff@���@��-@�/@��`@��@�1'@�+@��+@�M�@�$�@��@�O�@�r�@���@�33@�@��@�Ĝ@�Q�@��
@���@�t�@�;d@���@��R@��+@�E�@��#@�`B@�Ĝ@��w@�S�@��@�-@��7@�hs@�V@�Z@�33@�M�@�J@��@��@��@���@��@�j@�j@��@��`@��/@��j@�bN@���@��P@�l�@�
=@��@�\)@��R@�@�{@��@��@��7@��@�z�@�r�@�  @���@�+@��!@�^5@�~�@��+@���@��@�
=@��!@�^5@���@�z�@�z�@�1'@�+@�@���@�ff@��@���@��T@���@��h@�p�@�O�@�?}@��/@��u@��u@�r�@�1'@���@��w@��@��@�t�@�S�@�33@���@��R@��\@���@��!@��!@���@���@��\@�ff@�V@�-@�{@��@�O�@���@���@��j@��@�Z@�9X@�b@��;@�ƨ@��@�|�@�t�@�dZ@�33@���@��@��@��@�ȴ@�~�@�^5@��@��-@�X@�7L@��@��`@�r�@��@��
@���@��@�l�@�K�@�+@��@��\@�{@���@��h@��j@���@��D@�Z@�  @���@��w@�l�@�
=@��R@�-@��@���@�V@��u@��@�z�@�j@�j@�Z@�9X@� �@��m@���@�;d@��@��y@���@��+@�5?@��-@��7@�`B@�G�@�%@��@��@��`@��/@���@��D@�9X@� �@��w@�K�@���@��@���@��+@�n�@�=q@�{@�{@�{@�@��T@��#@��-@��@�r�@�Q�@�9X@�(�@��@�1@�  @��@��
@���@�\)@�@��@��y@��@���@��!@�^5@��@��#@���@�hs@�7L@�/@��@�Z@�A�@�b@��@
=@~��@}��@}p�@}V@|��@|j@|9X@|�@{ƨ@{"�@z^5@z�@y��@y��@y�@y�#@y�7@y�@x�`@x�@w�@w�P@w|�@w;d@v��@v��@vff@u�-@t��@s��@s��@s�@sC�@r�H@r^5@q�@q7L@p�u@o�;@o�@n��@n��@nv�@n$�@m?}@l��@l�j@lI�@k�m@k��@kC�@k@j�\@jM�@j-@i��@i�@iX@i&�@h��@hQ�@g�@gl�@g;d@g�@fȴ@fE�@e�h@e�@dj@d(�@c��@c��@c�m@c�
@c�@cS�@cC�@co@b�!@b-@b-@b�@bJ@a�#@aX@a�@`�9@`bN@`A�@_�;@_�@^�y@^V@^@]�@]�T@]��@]O�@\��@\I�@[��@Z�H@Zn�@Z-@Y��@Yx�@Yx�@Y&�@Y�@X��@X��@X1'@W�w@W�P@Wl�@Vȴ@VE�@V{@U��@U�h@U`B@T�j@T��@Tj@T9X@S��@S"�@R��@R�!@RM�@Q�#@QX@P��@P�9@Pr�@P1'@O�@O;d@O
=@Nȴ@NE�@M@M�-@Mp�@M?}@L�/@L�D@Lz�@L9X@K��@K�F@J�@J�\@J^5@JM�@J�@I��@I�#@I��@I�^@Ix�@IX@I&�@H��@H��@HQ�@H �@H  @G�@G��@G�w@G|�@G;d@F�@F�+@FE�@E��@E��@E`B@Dz�@D�@C�
@C�F@C��@C��@CdZ@B��@BJ@A��@AG�@A%@@�`@@bN@?��@?+@>ff@=p�@<��@<�@<��@<��@<z�@<j@<9X@<1@;dZ@:��@:~�@:-@9�@9��@9%@8�`@8��@8r�@8A�@7�w@7K�@6�y@6ȴ@6ȴ@6E�@5�@5?}@5V@4�j@4z�@4j@4I�@4�@3��@2�\@2-@1��@1&�@0�9@0�@0bN@0A�@01'@0b@/��@/K�@.��@.��@.v�@.V@.5?@.$�@.$�@.@-��@-�-@-`B@,�@,�@,��@,Z@,(�@+�m@+�
@+ƨ@+t�@+33@+@*�H@*��@*�@)�^@)��@)hs@)&�@(��@(1'@'�w@'|�@&��@&�+@&ff@&$�@%�h@%�@%p�@%O�@%V@$�/@$�j@$z�@#��@#��@#�@#S�@#@"�H@"~�@"-@!�@!��@!�7@!�@ �@   @��@��@K�@��@��@v�@E�@�T@�@?}@/@/@V@�/@��@��@�@��@�D@z�@j@I�@9X@9X@(�@1@ƨ@�F@��@��@t�@S�@o@@�H@��@n�@�@��@x�@��@�9@r�@A�@ �@�@�w@��@�P@l�@\)@K�@�@��@��@E�@$�@{@{@�T@�-@��@��@�h@`B@?}@/@�/@��@��@�D@z�@9X@��@33@�@��@n�@=q@�@�@��@��@�7@x�@X@G�@&�@%@��@�9@bN@1'@  @��@�@�P@l�@K�@
=@�y@v�@��@�+@V@E�@E�@$�@@��@@@@��@`B@`B@?}@��@�/@�j@�@�D@z�@z�@j@I�@I�@(�@9X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BK�BK�BK�BK�BL�BL�BL�BL�BL�BM�BM�BM�BM�BM�BN�BN�BN�BO�BR�BS�B[#B`BBdZBffBgmBp�Bu�Bv�B}�B�\B��B��B��B��B��B�9B��B�B�5B�B�B�ZB�B�yB�5B�`B�B�sB�BB�BB�TB�5B�;B��B�B�5B�5B�5B�B	7BDB1B��B�B��B1B	7B\BB  BPB\B+B�B�mB�B�yB�
B�B��B��B�jB�?B��B�VBl�B`BBW
By�B�Bv�B�Bq�BXB<jBK�B>wB9XB$�BhBPB
�B
�;B
ɺB
�B
�)B
�B
��B
B
��B
�B
�B
{�B
l�B
aHB
0!B
/B
�B
�B
�B
hB
B	��B	�B	�)B	��B	�LB	�B	�LB	�wB	�9B	��B	�hB	�B	`BB	O�B	\)B	gmB	hsB	dZB	ZB	G�B	D�B	A�B	B�B	?}B	2-B	�B	49B	2-B	-B	(�B	�B	B��B��B�NB�/B�TB�BÖB��B�3B�jBB�}B�dB�wB�XB�B�oBx�B��B��B��B��B��B�uB�%B��B��B�{B�B�B�bB�B~�B|�B|�Bn�BaHBy�B�B|�Bt�Bl�Bn�Bn�Br�Bn�B^5BT�BS�BQ�BP�BS�B^5BaHBaHB\)B[#BW
B^5BdZBcTBbNB_;B[#B\)BVBT�BC�BQ�BR�B>wBYBW
BH�BXB]/B]/B[#B[#B[#BT�BQ�BQ�B\)BR�BdZBC�BG�BO�B\)BYBXBZBYBXBW
BM�BB�BYB\)B_;BXBS�B^5Bm�Bl�Bk�Bn�Bo�Bk�BffBm�Bl�BjBu�Bv�Bv�B{�B}�B}�B}�B|�B�7B�1B�B�B�7B�PB�PB�bB�hB�hB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�XB�RB�RB�?B�FB�qB�qB�}BȴB��B��B�
B�)B�/B�5B�;B�HB�TB�TB�ZB�`B�sB�B��B��B��B	B	
=B		7B	%B	+B	uB	�B	$�B	&�B	&�B	%�B	%�B	&�B	/B	1'B	5?B	9XB	;dB	;dB	:^B	<jB	=qB	<jB	@�B	G�B	F�B	E�B	M�B	Q�B	N�B	S�B	T�B	S�B	]/B	^5B	cTB	dZB	e`B	gmB	k�B	s�B	s�B	u�B	y�B	v�B	w�B	x�B	t�B	�B	�B	|�B	�%B	�+B	�=B	�JB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�FB	�dB	�jB	�wB	��B	��B	B	B	ÖB	ÖB	ÖB	ÖB	B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�
B	�B	�#B	�5B	�5B	�/B	�)B	�5B	�BB	�NB	�TB	�ZB	�ZB	�ZB	�TB	�TB	�ZB	�mB	�sB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B	��B
B
B
B
+B
+B
+B
1B

=B

=B
	7B
	7B
	7B
+B
B
B

=B
DB
JB
JB
JB
JB
JB
DB

=B

=B
DB
PB
VB
VB
PB
PB
JB
JB
PB
VB
VB
VB
bB
VB
bB
oB
oB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
 �B
�B
#�B
#�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
%�B
&�B
&�B
%�B
&�B
(�B
(�B
(�B
'�B
'�B
&�B
(�B
(�B
+B
,B
-B
-B
,B
,B
,B
-B
,B
,B
,B
.B
.B
.B
-B
,B
.B
.B
.B
/B
.B
.B
-B
/B
0!B
2-B
1'B
1'B
/B
.B
0!B
0!B
0!B
1'B
33B
33B
49B
5?B
49B
5?B
49B
33B
33B
49B
5?B
5?B
49B
49B
6FB
6FB
7LB
7LB
6FB
8RB
8RB
7LB
6FB
7LB
9XB
9XB
8RB
8RB
8RB
:^B
;dB
;dB
;dB
;dB
9XB
<jB
<jB
;dB
<jB
>wB
>wB
>wB
=qB
>wB
?}B
?}B
?}B
>wB
=qB
@�B
B�B
B�B
A�B
B�B
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
D�B
D�B
C�B
C�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
B�B
E�B
F�B
G�B
G�B
G�B
F�B
D�B
E�B
G�B
G�B
H�B
H�B
G�B
F�B
F�B
F�B
G�B
K�B
M�B
M�B
L�B
M�B
M�B
L�B
L�B
K�B
K�B
M�B
N�B
M�B
N�B
M�B
O�B
O�B
O�B
O�B
N�B
O�B
P�B
Q�B
Q�B
P�B
O�B
R�B
R�B
S�B
S�B
T�B
S�B
S�B
Q�B
P�B
S�B
S�B
T�B
VB
W
B
W
B
W
B
W
B
W
B
VB
W
B
W
B
W
B
XB
YB
YB
YB
YB
YB
XB
XB
XB
W
B
YB
YB
ZB
YB
ZB
[#B
[#B
ZB
ZB
ZB
ZB
ZB
YB
ZB
ZB
[#B
[#B
ZB
ZB
YB
[#B
ZB
[#B
]/B
]/B
\)B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
_;B
`BB
`BB
`BB
aHB
`BB
aHB
bNB
cTB
bNB
bNB
bNB
cTB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
hsB
iyB
iyB
iyB
hsB
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
iyB
iyB
jB
jB
jB
iyB
jB
jB
jB
jB
jB
jB
iyB
iyB
iyB
jB
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
n�B
m�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
s�B
r�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
t�B
u�B
u�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BK�BK�BK�BK�BL�BL�BL�BL�BL�BM�BM�BM�BM�BM�BN�BN�BN�BO�BR�BTB[#B`BBdtBf�Bg�Bp�Bu�Bv�B~]B�\B�mB��B��B��B��B�nB��B�)B�B�B�B�2B�B�B�B��B��B�_B��B�ABB�B�'B��BյB��BߤB��B�'B�B	RB�B�B��B�B��B	7B
	B�B�B�B�B�B�G�O�B�KBܒB�6B�B�kB�hBĶB��B��B�B�Bp�BcTBZ�B{�B��Bx�B��Bs�B[WB@4BL�B@4B:�B'RB,B�B
�zB
�NB
�B
�KB
��B
��B
�B
��B
��B
�lB
�B
~�B
o�B
eFG�O�B
2�B
�B
�B
B
�B
�B	�B	ٴB	�B	�BB	��B	�'B	�8B	��B	�%B	�`B	�,B	�mG�O�B	TB	^B	h
B	h�B	d�B	[WB	J	B	FB	B�B	C-B	@4B	4B	�B	4TB	2�B	-�B	)�B	�B	9B�B�2B�B�VB�tB�+B�?B��B�`B��B�{B��B��B��B�*B�G�O�B}VB��B�!B�$B�nB��B�2B��B�WB�5B��B�zB��B�4B�B��B~�B~]Bq�Bd�Bz�B�oB}�BvFBn}Bp;BpUBs�Bp;B`�BW$BU�BS�BR�BUMB^�Ba�Ba�B\�B\)BXyB^�Bd�Bc�Bb�B_�B\)B\�BW$BVBE�BR�BT�G�O�BZBX_BJ�BX�B]dB]~B[�B[�B[�BU�BSBS�B]~BT�BfLG�O�BI�BP�B\]BY�BX�BZ�BY�BX�BW�BOBBDgBY�B\�B_�BYKBU�B_;Bm�Bl�Bl"Bo Bo�Bl"BgRBn/Bm]Bk�BvFBwfBw�B|PB~]B~wB~�B}�B�RB��B��B�'B��B��B��B��B��B� B��B��B�B�EB�CB�VB�HB�tB�>B�KB�B�]B�cB��B��B��B�rB��B��B��B�B��B�(B�OB�RB�&BՁB�sB�]B�dB�jB�pB�|B�B�B��B��B�B�kB�2B�^B�wB	�B	
XB		�B	�B	B	B	�B	$�B	'B	'B	&LB	&fB	'mB	/B	1'B	5?B	9�B	;�B	;�B	:�B	<�B	=�B	<�B	@�B	G�B	G+B	F%B	M�B	Q�B	OBB	TB	U�B	T�B	]dB	^�B	c�B	d�B	e�B	g�B	k�B	s�B	s�B	u�B	y�B	wB	xB	yXB	u�B	�;B	�UB	}�B	�YB	��B	�rB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�&B	�$B	�B	�6B	�6B	�)B	�5B	�UB	�[B	�zB	�dB	�jB	�wB	��B	��B	ªB	��B	ðB	ðB	ðB	��B	�B	��B	��B	�B	�B	��B	��B	�B	�.B	�B	� B	��B	�B	�B	�,B	�2B	�B	�+B	�+B	�?B	�SB	�EB	�YB	�kB	�qB	�OB	�OB	�~B	�xB	ޞB	��B	�hB	�nB	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�*B	��B	�>B	�XB	�B	�B	�B	�<B	�B
 B
 B
 B	�B	�(B	�(B	�B	�]B	�HB
UB
9B
gB
EB
EB
EB
KB

=B

=B
	RB
	RB
	lG�O�G�O�B
�B

rB
^B
~B
~B
dB
dB
~B
^B

rB

�B
xB
jB
�B
pB
jB
jB
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
"�B
!�B
 �B
 'B
#�B
$B
#B
$B
$�B
%B
$�B
%B
%�B
'B
'B
'B
&2B
'B
'B
&B
'B
)B
)B
)B
(>B
($B
'8B
)DB
)*B
+6B
,"B
-)B
-B
,"B
,"B
,"B
-)B
,=B
,WB
,=B
.B
./B
./B
-)B
,WB
./B
.IB
./B
/5B
.IB
.IB
-]B
/iB
0;B
2-B
1[B
1AG�O�B
.}B
0oB
0oB
0oB
1vB
3MB
3hB
4nB
5?B
4TB
5ZB
4TB
3hB
3�B
4�B
5ZB
5ZB
4nB
4�B
6`B
6`B
7fB
7fB
6�B
8�B
8�B
7fB
6�B
7�B
9rB
9rB
8�B
8�B
8�B
:xB
;B
;B
;B
;�G�O�B
<�B
<�B
;�B
<�B
>wB
>�B
>�B
=�B
>�B
?�B
?�B
?�B
>�B
=�B
@�B
B�B
B�B
A�B
B�B
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
D�B
D�B
C�B
C�B
B�B
C�B
C�B
C�B
C�B
D�B
D�G�O�B
E�B
F�B
G�B
G�B
G�B
F�G�O�B
E�B
G�B
G�B
H�B
H�B
G�B
F�B
GB
F�B
G�B
K�B
M�B
M�B
L�B
M�B
M�B
L�B
L�B
LB
K�B
M�B
N�B
M�B
OB
N"B
O�B
PB
O�B
O�B
O(B
PB
Q B
RB
RB
QB
PHB
S&B
SB
TB
T,B
T�B
TB
TB
RTG�O�B
T,B
T,B
U2B
V9B
W$B
W$B
W$B
W$B
W$B
VSB
W$B
W?B
W$B
XEB
Y1B
YKB
YB
YB
Y1B
XEB
XEB
X+B
WYB
Y1B
Y1B
Z7B
Y1B
Z7B
[=B
[=B
ZQB
Z7B
Z7B
Z7B
Z7B
YKB
Z7B
Z7B
[=B
[=B
ZQB
ZkB
YKB
[WB
ZkB
[qB
]dB
]IB
\]B
^5B
^OB
^OB
^OB
^OB
^OB
^OB
]dB
_pB
`\B
`\B
`\B
abB
`vB
abB
bhB
c�B
bhB
b�B
b�B
c�B
ezB
ezB
ezB
ezB
ezB
f�B
f�B
f�B
g�B
h�B
iyB
iyB
i�B
h�B
iyB
i�B
i�B
i�B
iyB
i�B
jB
j�B
jB
j�B
jB
i�B
i�B
jB
j�B
jB
i�B
j�B
j�B
j�B
j�B
j�B
j�B
i�B
i�B
i�B
j�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
n�B
m�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
s�B
r�G�O�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
t�B
u�B
u�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111411111111111111111141111111111111111111111111111111111141111111111111111111111111111111111111111111111111111141111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111114111111111111111111111111111111111111111114111111411111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801120034032018011200340320180112003403201806221324352018062213243520180622132435201804050727532018040507275320180405072753  JA  ARFMdecpA19c                                                                20180108093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180108003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180108003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180108003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180108003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180108003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180108003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180108003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180108003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180108003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20180108005524                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180108153437  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180109000000  CF  PSAL_ADJUSTED_QCC  D� G�O�                JM  ARCAJMQC2.0                                                                 20180111153403  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180111153403  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222753  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042435  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                