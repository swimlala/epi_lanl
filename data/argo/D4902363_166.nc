CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-07T00:35:10Z creation;2017-10-07T00:35:14Z conversion to V3.1;2019-12-19T07:59:53Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171007003510  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_166                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�+�:Ӏ1   @�+���J @:ѩ��l��d��-1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh��Bn��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��H@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_Bh�\Bn�\BwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DB�DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D��D��D�>D�~D��D��D�AHD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�:�D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�z�Dܺ�D��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D�HD�>D��HD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A�{A�{A�{A�{A�oA�oA�oA�oA�oA�{A�{A��A��A��A��A��A��A�{A�A�A�bA�
=A���A�ZAӡ�A�I�A�Aǡ�A�&�A�JA���A�G�A���A��PA���A���A���A���A�9XA�n�A�-A� �A�r�A�l�A�?}A��HA�ZA�-A���A��wA��A�  A�ZA��A��A��A�A�A��A�oA��FA�l�A�z�A��7A��A�ffA��-A��yA��jA���A�z�A�1A��wA���A��A���A�ȴA��PA�ZA�1A�;dA��FA���A���A�bA���A��PA��7A��A���A�bNA�?}A�+A��A�K�A~�A|��A{p�Az�\AydZAx��Aw�FAv��Av-Au�As�Ar��Ar�+ArQ�Ap�HApI�Ao��AoVAn�+Al�AlZAk��AkK�Ak%AjĜAjn�Aj�Ait�Ah�+Af��Af=qAe�;Ae��Ae%Ad=qAb�/A`bA_��A_XA_A^A\��A[S�AZ��AZ(�AY/AX�/AWhsAVM�AU`BAT��AR�HAQ��AP��APĜAP��AP��AP~�AOl�ANM�AMXAL�jAL~�ALA�AL{AK�PAJ��AJv�AIAH��AHE�AH$�AGAFjAE�^AD��AD^5AB��AB �AA�;A@ȴA??}A>z�A=�wA=dZA<��A;�;A;XA:�uA9`BA9oA8�A8��A7�A7+A6�/A6�HA5�mA4r�A4A2��A1��A0�`A0ffA/A/S�A/A-+A+�#A*�`A*v�A*$�A)��A)hsA(��A'�A'p�A&��A&1'A%�hA$��A$ĜA$A�A#�wA#"�A!�#A ��A�AI�A%A�mA�^A�7A`BA�A��AO�A�Av�A��A`BA�mA�\A=qA�A��A�wA�PA�AI�A�A��A�A�;A��A�;A��A��A^5A�hA
��A
M�A
 �A	��A�
A�A��A1A
=A�uAoA ��A 9X@��@��/@���@��@�-@� �@�I�@�!@���@�@�!@�~�@���@�@�@�`B@���@�\)@�E�@�hs@��@�-@�%@�9X@�\)@�M�@�{@��@�E�@ش9@��y@Չ7@ԃ@�K�@�=q@��@ЋD@��m@�|�@��@��@̼j@̋D@�z�@�Q�@��m@���@��T@�hs@�7L@�7L@�V@�Z@��m@��@��#@�V@�I�@å�@�;d@��@��T@���@��j@�  @�@�$�@��7@��@�r�@�C�@���@��/@��@�K�@�O�@��`@�bN@��@��@��@��@�1'@�;d@���@�5?@���@���@���@�+@��H@�n�@��-@�/@��@��j@� �@�t�@�\)@���@�J@�/@�1@�"�@�~�@���@���@�x�@�Ĝ@�Q�@�1'@��!@�$�@���@�%@��D@�b@�|�@�o@�E�@��@�r�@�O�@�Q�@���@��R@�@��T@��-@�p�@�%@���@��m@��@�5?@���@�hs@���@��9@��D@�1'@��;@��P@���@�+@�v�@���@���@�O�@��@���@��u@�r�@���@�dZ@�K�@��H@�=q@�@���@��h@���@�G�@��@�O�@�/@��@���@��h@���@���@�j@�1'@�b@�1@���@�@�V@�@�hs@�%@���@���@�  @���@���@��@�l�@�S�@�;d@�+@�o@���@�ff@�5?@�{@�@�hs@��@�Q�@�I�@�I�@�b@K�@~�y@~v�@~@}�-@}p�@}O�@|�@|�@|I�@{dZ@y��@yG�@x�`@x�@x  @w�@w|�@w;d@w
=@v�@v��@v$�@u@u�-@u�-@u��@u��@u��@u�@t�@t9X@s33@r��@rJ@q�^@q��@q��@q��@q��@q�7@q7L@p��@p�9@p1'@o�;@p  @pb@ol�@m�@m/@lI�@kdZ@k"�@j��@jn�@jJ@i��@ix�@h�`@h�9@hbN@hb@g�@g��@g;d@f�@f�+@fff@fE�@f{@f{@e�T@e�h@e�@d�@c�@b��@b�@a�@a��@a�7@`��@`A�@_�@^�+@^�+@^{@]�T@^$�@^��@^��@^$�@]�@]��@]p�@]p�@]/@\�@\��@\��@\1@[ƨ@[��@[t�@Z�H@Zn�@Z�@ZJ@Y��@Y�^@Y�@X�`@X�u@X�@Xr�@W�@Wl�@W
=@V�y@V�y@V�@V{@U�h@U�@Up�@Up�@U`B@UO�@U/@T��@T�@S��@S��@SdZ@SC�@So@R��@R�!@R�!@R��@R��@RJ@Q%@N��@Nff@NE�@N$�@M��@M@M��@MV@L��@L�D@L�D@Lz�@L1@K��@J�@Jn�@J^5@J^5@J^5@J^5@JM�@J-@I�@I�7@H��@HA�@G��@G+@FV@F$�@E��@Ep�@D�D@C�m@C��@C��@CdZ@Co@B��@BM�@BJ@A��@A��@Ax�@AG�@@��@@A�@?�w@?�P@?l�@?K�@?
=@>�y@>��@>5?@=��@=�@=?}@<�@<��@<�D@<(�@<1@;�
@;�@;dZ@;o@:~�@:=q@:J@9��@9��@9�7@9hs@9��@9��@9��@9��@9�7@9G�@97L@8��@8�`@8�`@8��@8Ĝ@8Ĝ@8Ĝ@8�`@8Ĝ@8A�@7�;@7��@7��@7\)@7+@6��@6�@6ȴ@6�R@6E�@5/@4��@4��@4z�@4j@4Z@3��@3o@2��@2-@1x�@17L@1&�@1�@1%@0�`@0��@0Ĝ@0�@0bN@01'@0  @/�@/��@/\)@/
=@.ȴ@.ff@.E�@.V@.V@.E�@-��@-p�@-?}@,�@,�j@,�D@+��@+ƨ@+�F@+ƨ@+�F@+�@+t�@+S�@*�H@*-@)��@)��@)7L@(Q�@'��@'��@'�P@'�@&E�@&@%�@%�@%�@%�@%?}@$�@$9X@#�
@#�@#t�@#S�@#@"~�@"^5@"�@!�#@!��@!��@!��@!�7@!x�@!X@!&�@ �`@ �u@ r�@ 1'@  �@   @��@|�@+@�@��@�+@{@�-@O�@`B@`B@O�@?}@?}@��@��@�D@z�@��@Z@9X@�@��@��@�@X@�@��@��@�9@��@��@r�@b@�P@;d@�@ȴ@��@��@�R@�@�y@�y@�y@�R@{@�T@��@�h@O�@�@��@�D@Z@(�@(�@�m@t�@��@~�@^5@^5@�@J@J@J@�@�@�^@��@�7@X@&�@�`@��@%@%@�@�@�@��@��@�9@�9@�9@�9@�9@Q�@  @��@|�@K�@+@�@�@�y@�R@��@v�@5?@$�@$�@$�@@�@�@��@�@�@9X@�m@�F@t�@C�@"�@
�@
��@
�!@
�\@
n�@
�@	��@	�7@	hs@��@�u@r�@Q�@1'@ �@b@�@�w@��@��@�P@�P@l�@\)@\)@;d@
=@�y@�@��@V@E�@$�@�@@��@��@�@/@V@�@�@�@�D@I�@��@��@�@dZ@S�@o@�H@��@n�@M�@�@�#@�^@��@��@��@��@�7@x�@X@%@ �`@ ��@ �911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A�{A�{A�{A�{A�oA�oA�oA�oA�oA�{A�{A��A��A��A��A��A��A�{A�A�A�bA�
=A���A�ZAӡ�A�I�A�Aǡ�A�&�A�JA���A�G�A���A��PA���A���A���A���A�9XA�n�A�-A� �A�r�A�l�A�?}A��HA�ZA�-A���A��wA��A�  A�ZA��A��A��A�A�A��A�oA��FA�l�A�z�A��7A��A�ffA��-A��yA��jA���A�z�A�1A��wA���A��A���A�ȴA��PA�ZA�1A�;dA��FA���A���A�bA���A��PA��7A��A���A�bNA�?}A�+A��A�K�A~�A|��A{p�Az�\AydZAx��Aw�FAv��Av-Au�As�Ar��Ar�+ArQ�Ap�HApI�Ao��AoVAn�+Al�AlZAk��AkK�Ak%AjĜAjn�Aj�Ait�Ah�+Af��Af=qAe�;Ae��Ae%Ad=qAb�/A`bA_��A_XA_A^A\��A[S�AZ��AZ(�AY/AX�/AWhsAVM�AU`BAT��AR�HAQ��AP��APĜAP��AP��AP~�AOl�ANM�AMXAL�jAL~�ALA�AL{AK�PAJ��AJv�AIAH��AHE�AH$�AGAFjAE�^AD��AD^5AB��AB �AA�;A@ȴA??}A>z�A=�wA=dZA<��A;�;A;XA:�uA9`BA9oA8�A8��A7�A7+A6�/A6�HA5�mA4r�A4A2��A1��A0�`A0ffA/A/S�A/A-+A+�#A*�`A*v�A*$�A)��A)hsA(��A'�A'p�A&��A&1'A%�hA$��A$ĜA$A�A#�wA#"�A!�#A ��A�AI�A%A�mA�^A�7A`BA�A��AO�A�Av�A��A`BA�mA�\A=qA�A��A�wA�PA�AI�A�A��A�A�;A��A�;A��A��A^5A�hA
��A
M�A
 �A	��A�
A�A��A1A
=A�uAoA ��A 9X@��@��/@���@��@�-@� �@�I�@�!@���@�@�!@�~�@���@�@�@�`B@���@�\)@�E�@�hs@��@�-@�%@�9X@�\)@�M�@�{@��@�E�@ش9@��y@Չ7@ԃ@�K�@�=q@��@ЋD@��m@�|�@��@��@̼j@̋D@�z�@�Q�@��m@���@��T@�hs@�7L@�7L@�V@�Z@��m@��@��#@�V@�I�@å�@�;d@��@��T@���@��j@�  @�@�$�@��7@��@�r�@�C�@���@��/@��@�K�@�O�@��`@�bN@��@��@��@��@�1'@�;d@���@�5?@���@���@���@�+@��H@�n�@��-@�/@��@��j@� �@�t�@�\)@���@�J@�/@�1@�"�@�~�@���@���@�x�@�Ĝ@�Q�@�1'@��!@�$�@���@�%@��D@�b@�|�@�o@�E�@��@�r�@�O�@�Q�@���@��R@�@��T@��-@�p�@�%@���@��m@��@�5?@���@�hs@���@��9@��D@�1'@��;@��P@���@�+@�v�@���@���@�O�@��@���@��u@�r�@���@�dZ@�K�@��H@�=q@�@���@��h@���@�G�@��@�O�@�/@��@���@��h@���@���@�j@�1'@�b@�1@���@�@�V@�@�hs@�%@���@���@�  @���@���@��@�l�@�S�@�;d@�+@�o@���@�ff@�5?@�{@�@�hs@��@�Q�@�I�@�I�@�b@K�@~�y@~v�@~@}�-@}p�@}O�@|�@|�@|I�@{dZ@y��@yG�@x�`@x�@x  @w�@w|�@w;d@w
=@v�@v��@v$�@u@u�-@u�-@u��@u��@u��@u�@t�@t9X@s33@r��@rJ@q�^@q��@q��@q��@q��@q�7@q7L@p��@p�9@p1'@o�;@p  @pb@ol�@m�@m/@lI�@kdZ@k"�@j��@jn�@jJ@i��@ix�@h�`@h�9@hbN@hb@g�@g��@g;d@f�@f�+@fff@fE�@f{@f{@e�T@e�h@e�@d�@c�@b��@b�@a�@a��@a�7@`��@`A�@_�@^�+@^�+@^{@]�T@^$�@^��@^��@^$�@]�@]��@]p�@]p�@]/@\�@\��@\��@\1@[ƨ@[��@[t�@Z�H@Zn�@Z�@ZJ@Y��@Y�^@Y�@X�`@X�u@X�@Xr�@W�@Wl�@W
=@V�y@V�y@V�@V{@U�h@U�@Up�@Up�@U`B@UO�@U/@T��@T�@S��@S��@SdZ@SC�@So@R��@R�!@R�!@R��@R��@RJ@Q%@N��@Nff@NE�@N$�@M��@M@M��@MV@L��@L�D@L�D@Lz�@L1@K��@J�@Jn�@J^5@J^5@J^5@J^5@JM�@J-@I�@I�7@H��@HA�@G��@G+@FV@F$�@E��@Ep�@D�D@C�m@C��@C��@CdZ@Co@B��@BM�@BJ@A��@A��@Ax�@AG�@@��@@A�@?�w@?�P@?l�@?K�@?
=@>�y@>��@>5?@=��@=�@=?}@<�@<��@<�D@<(�@<1@;�
@;�@;dZ@;o@:~�@:=q@:J@9��@9��@9�7@9hs@9��@9��@9��@9��@9�7@9G�@97L@8��@8�`@8�`@8��@8Ĝ@8Ĝ@8Ĝ@8�`@8Ĝ@8A�@7�;@7��@7��@7\)@7+@6��@6�@6ȴ@6�R@6E�@5/@4��@4��@4z�@4j@4Z@3��@3o@2��@2-@1x�@17L@1&�@1�@1%@0�`@0��@0Ĝ@0�@0bN@01'@0  @/�@/��@/\)@/
=@.ȴ@.ff@.E�@.V@.V@.E�@-��@-p�@-?}@,�@,�j@,�D@+��@+ƨ@+�F@+ƨ@+�F@+�@+t�@+S�@*�H@*-@)��@)��@)7L@(Q�@'��@'��@'�P@'�@&E�@&@%�@%�@%�@%�@%?}@$�@$9X@#�
@#�@#t�@#S�@#@"~�@"^5@"�@!�#@!��@!��@!��@!�7@!x�@!X@!&�@ �`@ �u@ r�@ 1'@  �@   @��@|�@+@�@��@�+@{@�-@O�@`B@`B@O�@?}@?}@��@��@�D@z�@��@Z@9X@�@��@��@�@X@�@��@��@�9@��@��@r�@b@�P@;d@�@ȴ@��@��@�R@�@�y@�y@�y@�R@{@�T@��@�h@O�@�@��@�D@Z@(�@(�@�m@t�@��@~�@^5@^5@�@J@J@J@�@�@�^@��@�7@X@&�@�`@��@%@%@�@�@�@��@��@�9@�9@�9@�9@�9@Q�@  @��@|�@K�@+@�@�@�y@�R@��@v�@5?@$�@$�@$�@@�@�@��@�@�@9X@�m@�F@t�@C�@"�@
�@
��@
�!@
�\@
n�@
�@	��@	�7@	hs@��@�u@r�@Q�@1'@ �@b@�@�w@��@��@�P@�P@l�@\)@\)@;d@
=@�y@�@��@V@E�@$�@�@@��@��@�@/@V@�@�@�@�D@I�@��@��@�@dZ@S�@o@�H@��@n�@M�@�@�#@�^@��@��@��@��@�7@x�@X@%@ �`@ ��@ �911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B�dB�B�B�BB�#B��B�3B�hBu�B[#B;dB(�B�BDBBBB  BbBVB{B�B�BhB	7B��B�`B�/B�#B�B�B��B��B��BȴB�qB�!B��B�\B� Bq�Br�Bt�Bs�Bp�BiyBZB`BB_;BYBM�B;dB8RB1'B$�B�BoB  B
��B
�B
�5B
ŢB
�RB
�XB
�FB
�3B
�B
��B
�oB
�7B
~�B
w�B
t�B
m�B
iyB
cTB
[#B
XB
Q�B
G�B
A�B
C�B
@�B
7LB
49B
0!B
,B
'�B
�B
�B
�B
�B
�B
�B
uB
\B

=B
B	��B	��B	��B	��B	�B	�B	�ZB	��B	��B	��B	��B	ǮB	�qB	�?B	�9B	�B	��B	��B	��B	��B	�{B	�PB	�B	~�B	x�B	z�B	{�B	{�B	z�B	u�B	q�B	m�B	jB	iyB	hsB	gmB	dZB	`BB	^5B	ZB	S�B	N�B	M�B	G�B	B�B	>wB	7LB	33B	-B	(�B	$�B	�B	�B	�B	�B	�B	�B	VB	
=B	1B	B	B	B	B��B��B��B��B��B�B�B�fB�ZB�BB�5B�#B�
B��B��BĜB��B��B�}B�qB�dB�RB�9B�3B�!B�B�B��B��B��B��B��B��B��B�uB�uB�bB�VB�bB�bB�\B�VB�=B�7B�+B�B�B}�By�Bu�Bw�Bw�Bv�Bu�Bt�Bq�Bn�Bn�Bl�Bl�BiyBffBe`Be`BgmBffBbNB_;B]/B\)B[#BP�BO�BJ�BF�B@�BD�BA�BC�BA�B=qB;dB;dB;dB9XB6FB/B1'B1'B2-B2-B49B2-B/B0!B49B33B1'B0!B/B.B.B1'B0!B0!B/B0!B,B&�B)�B,B.B2-B2-B49B49B6FB5?B6FB5?B33B49B:^B;dB:^B9XB9XB;dB=qB?}B?}B>wB<jB<jB;dB;dB>wB@�BA�BC�BB�BF�BF�BD�BE�BF�BH�BJ�BJ�BK�BJ�BJ�BN�BP�BO�BN�BXBZB[#BZB\)B\)BaHBcTBffBhsBhsBgmBiyBn�Bp�Bp�Bq�Bs�Bv�Bt�Bu�Bu�Bx�Bv�Bu�Bu�Bx�Bw�B{�B~�B�B�B�B�B�B�B�B�%B�%B�B�B�B�B�B�B�%B�JB�\B�hB�bB�oB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�'B�?B�LB�^B�qB�wB��BBÖBŢBŢBŢBȴB��B��B��B��B�B�B�)B�/B�BB�ZB�mB�mB�mB�B�B�B�B��B��B��B��B��B��B��B��B	B	+B	1B	1B	JB	bB	bB	hB	oB	oB	uB	uB	uB	uB	�B	�B	�B	�B	�B	!�B	$�B	%�B	$�B	&�B	+B	,B	.B	1'B	5?B	7LB	8RB	:^B	;dB	<jB	=qB	B�B	D�B	E�B	G�B	H�B	I�B	J�B	K�B	L�B	L�B	M�B	N�B	P�B	P�B	P�B	P�B	P�B	P�B	P�B	R�B	T�B	ZB	\)B	_;B	aHB	cTB	e`B	gmB	gmB	hsB	k�B	n�B	q�B	u�B	w�B	w�B	w�B	w�B	y�B	z�B	}�B	~�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�1B	�DB	�JB	�\B	�bB	�hB	�bB	�bB	�bB	�bB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�9B	�FB	�FB	�FB	�RB	�XB	�XB	�XB	�^B	�dB	�qB	�qB	�wB	�}B	��B	ÖB	ŢB	ŢB	ĜB	ƨB	ȴB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�)B	�/B	�/B	�;B	�NB	�TB	�TB	�TB	�HB	�NB	�BB	�NB	�TB	�TB	�TB	�`B	�`B	�ZB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B

=B

=B
JB
PB
VB
VB
bB
bB
bB
oB
uB
{B
{B
�B
�B
�B
�B
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
!�B
!�B
!�B
 �B
�B
 �B
"�B
"�B
"�B
"�B
!�B
"�B
#�B
"�B
"�B
$�B
%�B
%�B
%�B
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
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
,B
-B
.B
.B
-B
.B
.B
.B
/B
0!B
0!B
/B
/B
1'B
2-B
2-B
1'B
1'B
33B
49B
5?B
49B
33B
49B
49B
5?B
6FB
6FB
7LB
7LB
6FB
7LB
8RB
8RB
9XB
:^B
:^B
<jB
<jB
=qB
=qB
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
@�B
@�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
H�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
J�B
K�B
L�B
L�B
M�B
N�B
O�B
P�B
P�B
P�B
P�B
O�B
P�B
P�B
O�B
P�B
P�B
O�B
P�B
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
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
XB
YB
YB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
^5B
_;B
`BB
`BB
`BB
_;B
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
cTB
cTB
cTB
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
gmB
hsB
hsB
hsB
iyB
hsB
hsB
iyB
iyB
jB
iyB
iyB
iyB
jB
jB
k�B
k�B
l�B
k�B
l�B
l�B
l�B
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
n�B
o�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�"B�)B�B�/B�qB�=B��B�B�RB�bB��B�,B�5B�$B�6B�$B{JBa�BBAB0oBB�B�BB�BaB�B�BgB�BEB�B�B��B�yBބB��B��B�YB�B�VB��BɺB��B�GB�)B��B��Bu%Bt9Bu%BtBq[Bj�B\�B`�B`vBZkBPB=�B9	B2GB&�BCB�BuB
�fB
�;B
�-B
ɠB
��B
��B
��B
��B
��B
�>B
�B
�xB
�oB
y>B
u�B
n�B
jeB
d�B
\xB
X�B
SB
I�B
B�B
C�B
A B
8�B
5B
0�B
,�B
(�B
VB
]B
WB
1B
B
B
�B
�B
)B
{B	��B	��B	�rB	�LB	�B	��B	�LB	��B	ԕB	�oB	ΊB	�B	�.B	��B	��B	�UB	�0B	��B	��B	�B	��B	��B	�B	�iB	y�B	z�B	|B	|6B	{dB	w2B	sB	n�B	k6B	i�B	h�B	g�B	e,B	a-B	^�B	[#B	U2B	OvB	NVB	IB	C{B	?�B	8lB	4B	.�B	)�B	%�B	!bB	 �B	�B	�B	7B	YB	�B	)B		RB	mB	�B	{B	�B��B��B�dB�^B�FB�OB�wB�$B�zB�bB�B��B��B�B�B�?BªB� B� B�B�B�>B�ZB�B�'B� B��B��B��B��B��B��B�WB�+B��B��B�B��B��B��B��B�B��B�#B��B��B�BB{�BwLBx8BxBwBv+BuZBr�Bo�Bo�Bm�Bm]Bj�Bg�Bf�Bf�Bg�BgBcnB`\B]�B\�B\]BS[BQ�BL�BG�BA�BE�BCGBDgBB[B>�B<�B<PB<B:DB8B1vB2|B2|B3B2�B4�B2�B0�B1'B4�B3�B2-B1B/�B/OB/iB1�B0�B0�B/�B0�B-)B(�B+6B-CB/B2�B3B5B5%B6�B5�B6�B5�B4B5%B:�B;�B:�B9�B:*B<B=�B?�B?�B>�B=B<�B<PB<6B?.BA;BB'BD3BCGBF�BGBEmBF?BG_BIRBK^BKDBLdBK�BK�BO�BQhBQ BPBX�BZ�B[�BZ�B\�B\�Ba�BdBf�Bh�Bi*BhXBj0Bo BqBqBrGBt9Bv�Bu?Bv`Bv`By	BwfBvzBv�By�Bx�B|jBcB�aB�{B��B��B��B�3B��B��B��B��B��B��B��B��B��B�tB�B��B��B��B��B��B��B��B�!B�@B�zB��B��B�qB�]B�]B�cB�IB�}B��B�vB�ZB��B��B��B�.B��B��B��BżB��B�%B�B�B�<B�\B�2B�EB�QB�CBݘB�\B�ZB�B�B�B�"B��B�'B�B��B��B�B�FB�LB�^B�]B��B	�B	_B	�B	�B	�B	}B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	/B	"B	$�B	%�B	%B	'8B	+6B	,WB	.IB	1[B	5ZB	7�B	8�B	:�B	;�B	<�B	>B	B�B	D�B	E�B	G�B	H�B	I�B	J�B	K�B	L�B	MB	NB	O(B	P�B	Q B	Q B	Q B	QB	QB	Q4B	S@B	UgB	ZQB	\]B	_VB	a|B	cnB	e`B	g�B	g�B	h�B	k�B	n�B	q�B	u�B	w�B	xB	x8B	xlB	zDB	{JB	~]B	.B	.B	HB	.B	�B	�UB	�AB	�3B	�MB	�9B	�?B	�?B	�YB	�fB	�xB	�dB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	�B	��B	��B	�B	�)B	�CB	�UB	�AB	�GB	�MB	�MB	�nB	�`B	�zB	�zB	�lB	��B	�rB	��B	��B	�B	��B	��B	��B	��B	��B	ðB	ŢB	żB	��B	��B	��B	��B	��B	��B	�	B	�"B	��B	��B	��B	��B	�,B	�2B	�MB	�YB	�QB	�)B	�CB	�dB	�dB	�VB	�NB	�TB	�nB	�B	�B	��B	��B	�B	�nB	�nB	�B	�zB	�B	�B	�B	�sB	�yB	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�	B	��B	�B	��B	�B	�B	�B	�B	�"B	�B	�B	�B	�<B	�.B
 OB
 B
 B
 B
 B
AB
AB
AB
GB
3B
9B
?B
_B
_B
EB
KB
KB
	RB

XB

rB
~B
jB
�B
�B
bB
}B
}B
oB
uB
{B
{B
�B
�B
�B
�B
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
!�B
!�B
!�B
 �B
 BB
 �B
"�B
"�B
#B
"�B
"4B
#B
$B
#B
# B
$�B
%�B
%�B
%�B
$�B
$�B
%B
$�B
%B
$�B
$�B
&B
%�B
%�B
%�B
'B
'B
)B
)B
)�B
*B
*KB
*0B
*0B
*0B
*B
*B
*0B
,=B
-)B
./B
./B
-)B
./B
.IB
.cB
/OB
0;B
0;B
/iB
/�B
1[B
2GB
2GB
1[B
1vB
3hB
49B
5?B
4TB
3hB
4TB
4�B
5tB
6`B
6zB
7fB
7fB
6zB
7�B
8lB
8lB
9rB
:^B
:�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
IB
G�B
G�B
IB
H�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
J�B
K�B
L�B
L�B
M�B
N�B
O�B
P�B
P�B
P�B
Q B
PB
Q B
Q B
PB
Q B
QB
O�B
QB
R B
R B
R B
RB
R B
S@B
S&B
S�B
S�B
TB
TB
S�B
S�B
S�B
TB
U2B
UB
UB
U2B
U2B
VB
W
B
X+B
YB
Y1B
Z7B
ZB
[=B
[=B
\)B
\CB
\CB
\)B
\)B
[WB
[=B
\]B
\]B
]IB
^OB
^5B
^OB
^jB
^jB
^OB
^OB
^OB
_VB
_;B
_;B
_VB
^jB
_pB
`vB
`\B
`vB
_pB
abB
abB
abB
bhB
bhB
bhB
bhB
bNB
bhB
bhB
bhB
b�B
c�B
c�B
c�B
d�B
e�B
ezB
ezB
ezB
e`B
ezB
f�B
f�B
ffB
ffB
ffB
f�B
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
hsB
g�B
h�B
h�B
h�B
iyB
h�B
h�B
i�B
i�B
jB
i�B
i�B
i�B
j�B
j�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
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
n�B
o�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<^҉<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710101050432017101010504320171010105043201806221231462018062212314620180622123146201804050427132018040504271320180405042713  JA  ARFMdecpA19c                                                                20171007093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171007003510  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171007003511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171007003512  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171007003513  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171007003513  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171007003513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171007003513  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171007003514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171007003514                      G�O�G�O�G�O�                JA  ARUP                                                                        20171007005546                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20171010014941  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20171010014921  CV  JULD            G�O�G�O�F�]�                JM  ARGQJMQC2.0                                                                 20171010014921  CV  JULD_LOCATION   G�O�G�O�F�]�                JM  ARGQJMQC2.0                                                                 20171010014921  CV  LATITUDE        G�O�G�O�A֑h                JM  ARGQJMQC2.0                                                                 20171010014921  CV  LONGITUDE       G�O�G�O��$�u                JM  ARCAJMQC2.0                                                                 20171010015043  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171010015043  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20171011000000  CF  PSAL_ADJUSTED_QCD�  D�  G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192713  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033146  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                