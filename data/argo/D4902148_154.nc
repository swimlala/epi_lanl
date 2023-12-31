CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-11-04T15:35:55Z creation;2018-11-04T15:35:59Z conversion to V3.1;2019-12-18T07:19:09Z update;2022-11-21T05:29:54Z update;     
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
_FillValue                 �  ]$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   PSAL_ADJUSTED_ERROR          
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181104153555  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_154                     2C  Dd!�NAVIS_A                         0397                            ARGO 011514                     863 @؎#� 1   @؎#β@�@<�T`�d��d!�_o�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,��D-u�D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}��D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D���D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�z�D���D��D�>D�~D��D��D�>D�~D��HD�HD�:�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��7A��hA��hA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��7A��A��+A��7A�t�A�l�A�t�A�x�A���A�^5A���A��/A���A�-A�p�A�5?A��A�A�A��;A�Q�A���A�/A�G�A���A�l�A��;A�ȴA�bA�$�A�XA��A���A�oA�|�A�?}A��A��wA�A�A��DA��+A��#A�z�A�A��A��A���A�z�A�ȴA��yA�bA���A�%A��A���A�Q�A���A��jA�|�A�G�A�7LA��wA�?}A�ffA��9A�&�A��jA�S�A�|�A���A�7LA�n�A���A�?}A��A}�hA{`BAx��AvȴAu��At�yAtn�As�As"�ArbNAq�
Ap~�An�An1'AlbNAi��Ah�DAgp�Af��Af$�Ae�Ac�^AcVAbZAat�A`5?A^ȴA]�FA\�`A[�7AY�hAX�AX��AWhsAV~�ASp�AP�\AO�FAN�`AN$�AM��AMdZAM"�AMoAM
=AL��AL��AL�AL�HAL��AK�AJ1AH��AHM�AGx�AFI�AD^5AA��A?��A>ffA<��A<n�A<-A;�FA:ZA8�HA8v�A8I�A8 �A7�
A7dZA6��A6=qA4�9A4 �A4JA3�A3��A2jA2JA1�A1��A1hsA0Q�A/��A/+A.�\A.  A-oA,E�A*�+A)33A(��A(jA'��A'p�A';dA&�A&�9A&~�A&Q�A&=qA&JA%"�A$5?A#O�A"�A!;dA �yA �A bAK�A��A��A\)A��A�+A5?A�A"�A��A��A��A1'A��AO�A�A�PAS�A�HAr�A�A�wA�FA��A�PAp�AK�A��A�DA�#Al�A�\A��A�FAjAG�Av�A1Al�A
��A
bNA
-A	��A	"�A�yA1AVA1A�Az�A�!A�mA��A V@�~�@�I�@���@�J@��`@�b@�l�@��H@��^@�w@���@�9@�bN@�\@�\)@�X@�F@�"�@��H@⟾@�n�@��@�|�@�E�@�G�@���@�\)@���@��@�p�@�I�@�+@�5?@���@Ϯ@��@��@�~�@�(�@�$�@�/@���@ŉ7@�@�-@���@���@��F@��#@�l�@��7@�?}@�7L@��/@�I�@���@���@��-@�7L@�r�@��P@�33@�ȴ@�M�@��/@�j@�dZ@���@�-@���@���@�  @��m@��
@���@��R@��^@��`@�1@���@�
=@��R@�J@�V@��;@�v�@��@��@�A�@� �@��@�33@�M�@���@�/@��@�(�@�t�@�ȴ@��^@���@�b@���@���@��@�dZ@�@���@�ff@�V@�=q@�-@��@���@���@�p�@��@���@�bN@��
@��y@��@�&�@�V@���@��u@�ƨ@���@�n�@�{@��#@���@�p�@�O�@�7L@�%@��`@��@�j@�Q�@�1'@��@�C�@��@�ȴ@��+@�5?@�$�@��@�@��^@���@�p�@�?}@���@��j@��u@�1'@���@�l�@�\)@�C�@�33@��@��@��R@�V@��#@��h@�p�@�X@�&�@��9@�r�@�Z@�I�@�1'@�ƨ@��m@��F@��P@�l�@�S�@�33@�o@���@���@��R@�~�@�M�@�-@�@���@�@���@��@��@��`@��@�9X@� �@�  @�P@
=@
=@~�y@~ȴ@~E�@}�h@}�h@}�h@}�h@}�@}O�@|z�@{��@{C�@z��@z�\@y��@y��@x��@x1'@w�@w�@v��@u@uO�@tZ@sS�@s@r�@r�@r�@r�H@r�H@r�H@r��@r�!@r=q@qX@p�`@pbN@p  @o\)@o
=@n��@n�@n�+@n5?@n$�@n{@n@m�@m�T@m�h@m�@l��@l1@k�m@k��@k"�@j�@j�!@i��@ix�@h��@h1'@g|�@g�@f�y@f�@f��@fff@e�@e@e�@e`B@d�@dj@d�@c�m@c�F@co@b�!@b��@b�\@bn�@a�^@aX@a%@`��@`�9@`Ĝ@`�u@`bN@`1'@_\)@^��@^ff@]��@]V@\Z@[��@[�@[t�@[S�@[C�@Z�H@Z~�@Z^5@Z=q@Z�@Y��@Y�@Y��@YX@XĜ@X�@XbN@XQ�@X1'@X  @W�;@W�P@W\)@W�@Vȴ@V��@V5?@U�@U/@T�@T�j@T�@T�@T�D@TZ@S�m@S�F@S��@SdZ@R�H@Q�@Q��@Qx�@P��@PbN@P  @O��@O�P@Ol�@O�@N��@N��@N��@N��@N�+@Nff@Mp�@L�@K@JM�@I�^@IX@I�@H��@H��@HbN@H1'@H1'@H �@G�@G�@G�w@GK�@G
=@Fȴ@FE�@E?}@D��@DZ@D9X@D�@C��@Cƨ@C��@C��@Ct�@CS�@C33@B�@B=q@A��@Ahs@A7L@A&�@@��@@�u@@�@@bN@@A�@@A�@@1'@@ �@?��@?K�@>�@>�R@>E�@>@=��@=��@=p�@=V@<�@<�@<�@<�@<�@<�/@<�D@<j@<9X@;ƨ@;�@;dZ@;o@:~�@:-@:J@9�#@9��@9��@9��@9��@9�7@9x�@9hs@9X@8��@8Q�@8b@7�w@7�P@7l�@7\)@7\)@7K�@7;d@7�@7
=@6��@6�@6��@6E�@6@5��@5?}@4z�@3��@3��@3o@2��@2��@2��@2n�@2M�@2=q@2-@1��@1��@1�7@1hs@1&�@1%@0��@0�`@0��@0�u@0 �@/�;@/��@/�w@/|�@/K�@/�@.��@.v�@.V@.@-��@-V@,��@,9X@+�m@+��@+�@+t�@+dZ@+S�@+"�@+@*��@*^5@*=q@*=q@*-@)��@)&�@(��@(bN@(Q�@(Q�@(A�@(A�@(b@'�@'�w@'�P@'K�@&�@&�R@&��@&v�@&5?@%�h@%?}@%V@$�@$��@$�j@$��@$z�@$(�@#�m@#t�@#33@#@"^5@"-@"J@!�@!�@!�#@!��@!�^@!��@!x�@!G�@!%@ Ĝ@ bN@   @�;@��@l�@K�@
=@
=@��@ȴ@v�@E�@5?@@�@V@�/@�/@z�@��@��@t�@t�@33@�H@��@�!@�!@~�@~�@n�@^5@-@�@J@J@��@��@�#@��@hs@�@��@bN@Q�@A�@b@�@�;@��@��@��@�@\)@��@��@ff@{@�T@�-@?}@Z@��@�
@ƨ@��@��@S�@33@o@�@��@~�@n�@-@��@�#@�#@��@x�@X@G�@7L@�@�`@��@Ĝ@�9@�@1'@b@b@�@�P@|�@l�@+@
=@�y@�y@��@E�@�@��@��@p�@O�@�@�@��@�j@z�@9X@(�@1@dZ@33@"�@@
��@
��@
��@
��@
n�@
=q@
J@
J@	��@	��@	�#@	�#@	�#@	�#@	��@	��@	��@	�7@	x�@	x�@	hs@	X@	X@	G�@	&�@	�@	�@	�@	�@��@��@�@r�@b@�w@�@�P@�P@l�@;d@+@+@+@�@�@�@�y@ȴ@�R@��@�+@v�@V@V@V@E�@��@`B@O�@?}@�@�@�@��@�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��7A��hA��hA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��7A��A��+A��7A�t�A�l�A�t�A�x�A���A�^5A���A��/A���A�-A�p�A�5?A��A�A�A��;A�Q�A���A�/A�G�A���A�l�A��;A�ȴA�bA�$�A�XA��A���A�oA�|�A�?}A��A��wA�A�A��DA��+A��#A�z�A�A��A��A���A�z�A�ȴA��yA�bA���A�%A��A���A�Q�A���A��jA�|�A�G�A�7LA��wA�?}A�ffA��9A�&�A��jA�S�A�|�A���A�7LA�n�A���A�?}A��A}�hA{`BAx��AvȴAu��At�yAtn�As�As"�ArbNAq�
Ap~�An�An1'AlbNAi��Ah�DAgp�Af��Af$�Ae�Ac�^AcVAbZAat�A`5?A^ȴA]�FA\�`A[�7AY�hAX�AX��AWhsAV~�ASp�AP�\AO�FAN�`AN$�AM��AMdZAM"�AMoAM
=AL��AL��AL�AL�HAL��AK�AJ1AH��AHM�AGx�AFI�AD^5AA��A?��A>ffA<��A<n�A<-A;�FA:ZA8�HA8v�A8I�A8 �A7�
A7dZA6��A6=qA4�9A4 �A4JA3�A3��A2jA2JA1�A1��A1hsA0Q�A/��A/+A.�\A.  A-oA,E�A*�+A)33A(��A(jA'��A'p�A';dA&�A&�9A&~�A&Q�A&=qA&JA%"�A$5?A#O�A"�A!;dA �yA �A bAK�A��A��A\)A��A�+A5?A�A"�A��A��A��A1'A��AO�A�A�PAS�A�HAr�A�A�wA�FA��A�PAp�AK�A��A�DA�#Al�A�\A��A�FAjAG�Av�A1Al�A
��A
bNA
-A	��A	"�A�yA1AVA1A�Az�A�!A�mA��A V@�~�@�I�@���@�J@��`@�b@�l�@��H@��^@�w@���@�9@�bN@�\@�\)@�X@�F@�"�@��H@⟾@�n�@��@�|�@�E�@�G�@���@�\)@���@��@�p�@�I�@�+@�5?@���@Ϯ@��@��@�~�@�(�@�$�@�/@���@ŉ7@�@�-@���@���@��F@��#@�l�@��7@�?}@�7L@��/@�I�@���@���@��-@�7L@�r�@��P@�33@�ȴ@�M�@��/@�j@�dZ@���@�-@���@���@�  @��m@��
@���@��R@��^@��`@�1@���@�
=@��R@�J@�V@��;@�v�@��@��@�A�@� �@��@�33@�M�@���@�/@��@�(�@�t�@�ȴ@��^@���@�b@���@���@��@�dZ@�@���@�ff@�V@�=q@�-@��@���@���@�p�@��@���@�bN@��
@��y@��@�&�@�V@���@��u@�ƨ@���@�n�@�{@��#@���@�p�@�O�@�7L@�%@��`@��@�j@�Q�@�1'@��@�C�@��@�ȴ@��+@�5?@�$�@��@�@��^@���@�p�@�?}@���@��j@��u@�1'@���@�l�@�\)@�C�@�33@��@��@��R@�V@��#@��h@�p�@�X@�&�@��9@�r�@�Z@�I�@�1'@�ƨ@��m@��F@��P@�l�@�S�@�33@�o@���@���@��R@�~�@�M�@�-@�@���@�@���@��@��@��`@��@�9X@� �@�  @�P@
=@
=@~�y@~ȴ@~E�@}�h@}�h@}�h@}�h@}�@}O�@|z�@{��@{C�@z��@z�\@y��@y��@x��@x1'@w�@w�@v��@u@uO�@tZ@sS�@s@r�@r�@r�@r�H@r�H@r�H@r��@r�!@r=q@qX@p�`@pbN@p  @o\)@o
=@n��@n�@n�+@n5?@n$�@n{@n@m�@m�T@m�h@m�@l��@l1@k�m@k��@k"�@j�@j�!@i��@ix�@h��@h1'@g|�@g�@f�y@f�@f��@fff@e�@e@e�@e`B@d�@dj@d�@c�m@c�F@co@b�!@b��@b�\@bn�@a�^@aX@a%@`��@`�9@`Ĝ@`�u@`bN@`1'@_\)@^��@^ff@]��@]V@\Z@[��@[�@[t�@[S�@[C�@Z�H@Z~�@Z^5@Z=q@Z�@Y��@Y�@Y��@YX@XĜ@X�@XbN@XQ�@X1'@X  @W�;@W�P@W\)@W�@Vȴ@V��@V5?@U�@U/@T�@T�j@T�@T�@T�D@TZ@S�m@S�F@S��@SdZ@R�H@Q�@Q��@Qx�@P��@PbN@P  @O��@O�P@Ol�@O�@N��@N��@N��@N��@N�+@Nff@Mp�@L�@K@JM�@I�^@IX@I�@H��@H��@HbN@H1'@H1'@H �@G�@G�@G�w@GK�@G
=@Fȴ@FE�@E?}@D��@DZ@D9X@D�@C��@Cƨ@C��@C��@Ct�@CS�@C33@B�@B=q@A��@Ahs@A7L@A&�@@��@@�u@@�@@bN@@A�@@A�@@1'@@ �@?��@?K�@>�@>�R@>E�@>@=��@=��@=p�@=V@<�@<�@<�@<�@<�@<�/@<�D@<j@<9X@;ƨ@;�@;dZ@;o@:~�@:-@:J@9�#@9��@9��@9��@9��@9�7@9x�@9hs@9X@8��@8Q�@8b@7�w@7�P@7l�@7\)@7\)@7K�@7;d@7�@7
=@6��@6�@6��@6E�@6@5��@5?}@4z�@3��@3��@3o@2��@2��@2��@2n�@2M�@2=q@2-@1��@1��@1�7@1hs@1&�@1%@0��@0�`@0��@0�u@0 �@/�;@/��@/�w@/|�@/K�@/�@.��@.v�@.V@.@-��@-V@,��@,9X@+�m@+��@+�@+t�@+dZ@+S�@+"�@+@*��@*^5@*=q@*=q@*-@)��@)&�@(��@(bN@(Q�@(Q�@(A�@(A�@(b@'�@'�w@'�P@'K�@&�@&�R@&��@&v�@&5?@%�h@%?}@%V@$�@$��@$�j@$��@$z�@$(�@#�m@#t�@#33@#@"^5@"-@"J@!�@!�@!�#@!��@!�^@!��@!x�@!G�@!%@ Ĝ@ bN@   @�;@��@l�@K�@
=@
=@��@ȴ@v�@E�@5?@@�@V@�/@�/@z�@��@��@t�@t�@33@�H@��@�!@�!@~�@~�@n�@^5@-@�@J@J@��@��@�#@��@hs@�@��@bN@Q�@A�@b@�@�;@��@��@��@�@\)@��@��@ff@{@�T@�-@?}@Z@��@�
@ƨ@��@��@S�@33@o@�@��@~�@n�@-@��@�#@�#@��@x�@X@G�@7L@�@�`@��@Ĝ@�9@�@1'@b@b@�@�P@|�@l�@+@
=@�y@�y@��@E�@�@��@��@p�@O�@�@�@��@�j@z�@9X@(�@1@dZ@33@"�@@
��@
��@
��@
��@
n�@
=q@
J@
J@	��@	��@	�#@	�#@	�#@	�#@	��@	��@	��@	�7@	x�@	x�@	hs@	X@	X@	G�@	&�@	�@	�@	�@	�@��@��@�@r�@b@�w@�@�P@�P@l�@;d@+@+@+@�@�@�@�y@ȴ@�R@��@�+@v�@V@V@V@E�@��@`B@O�@?}@�@�@�@��@�@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bt�Bu�Bv�Bv�Bv�Bx�B{�B�B�%B�Bx�BiyBI�B(�BPBB��B�mBƨB�^B�RB�FB�9B�B��B��B�\Bn�B^5BL�B?}B:^B�B%B��B�5B�
B��B��B�NB�`B�TB�TB�B�BB�B��BÖB�RB�LB�FB�FB�B��B�{B�1B~�Bu�Bl�BaHBXBS�BO�BK�BF�B6FB-B"�BB
�yB
�;B
�B
��B
ĜB
�^B
�!B
��B
��B
�hB
�7B
y�B
hsB
VB
I�B
@�B
;dB
7LB
2-B
.B
(�B
#�B
�B
VB
	7B	��B	�B	�TB	�)B	�
B	��B	��B	ĜB	�}B	�^B	�9B	�B	��B	��B	��B	�\B	�%B	�B	}�B	v�B	o�B	aHB	R�B	M�B	I�B	F�B	C�B	B�B	A�B	A�B	@�B	@�B	@�B	@�B	?}B	=qB	8RB	0!B	)�B	&�B	"�B	�B	uB	+B��B��B�B�B�B�B�`B�;B�/B�)B�#B�B�B��B��B��BɺBɺBȴBƨBB��B�}B�wB�jB�RB�?B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�VB�DB�1B�%B�B�B�B� B}�B{�By�Bx�Bv�Bu�Bs�Bq�Bn�Bk�Bk�BiyBhsBffBe`BcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBdZBbNB^5B\)B[#BYBW
BT�BR�BQ�BP�BN�BM�BK�BK�BH�BH�BH�BE�BB�BA�B@�B=qB:^B8RB6FB49B1'B/B-B-B-B-B,B,B+B'�B%�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�BuBbBhB�B�B!�B#�B#�B#�B$�B$�B#�B"�B"�B!�B�B �B"�B%�B%�B$�B$�B$�B%�B)�B.B.B.B/B0!B1'B49B6FB7LB7LB8RB8RB8RB8RB@�BA�BB�BB�BE�BG�BI�BJ�BK�BJ�BK�BN�BR�BW
BZB[#B]/B]/B^5BaHBffBm�Bq�Bt�Bw�Bw�Bw�Bz�B~�B�B�B�%B�1B�JB�VB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�9B�XB�qB�qB�wB�}BBǮB��B��B��B��B��B��B��B��B�B�
B�B�B�B�5B�BB�NB�TB�`B�sB�sB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	+B	
=B	DB	JB	PB	bB	uB	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	%�B	&�B	)�B	)�B	+B	,B	/B	0!B	1'B	49B	5?B	6FB	8RB	:^B	:^B	:^B	:^B	=qB	A�B	A�B	B�B	B�B	B�B	B�B	D�B	F�B	I�B	M�B	N�B	Q�B	S�B	YB	ZB	\)B	]/B	_;B	dZB	gmB	l�B	p�B	q�B	r�B	r�B	q�B	q�B	q�B	q�B	q�B	q�B	s�B	v�B	w�B	z�B	{�B	~�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�+B	�1B	�=B	�PB	�\B	�\B	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�3B	�FB	�LB	�RB	�RB	�XB	�RB	�XB	�^B	�^B	�qB	�wB	��B	B	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�`B	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
JB
JB
PB
PB
\B
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
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
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
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
M�B
M�B
M�B
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
VB
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
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
_;B
_;B
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
cTB
cTB
cTB
cTB
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
gmB
gmB
gmB
gmB
gmB
hsB
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
k�B
k�B
k�B
k�B
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bt�Bu�Bv�Bv�Bv�Bx�B|B�MB�B��B|�BpUBQ�B/�B�B�B�PB�OB��B�B�$B�fB��B��B�pB��B�aBq'Ba|BO�BA�B>�B!bB�B��BߊB��B�TB�FB�B��B�ZB��B�)B�hB�sB��B�gB��B�fB��B��B��B��B�mB�lB�OBw2Bn/BbhBX�BT�BP�BL�BH�B7�B/5B&�BfB
��B
�\B
�B
��B
�?B
�B
��B
��B
��B
��B
��B
|�B
k�B
XB
KB
AoB
<B
88B
2�B
/B
*B
%�B
�B
�B
�B	��B	�B	�B	�/B	��B	�B	бB	ŢB	��B	��B	��B	��B	�LB	�B	�kB	��B	�B	��B	�B	x�B	s3B	d@B	T,B	N�B	J�B	GEB	C�B	B�B	A�B	A�B	@�B	@�B	@�B	@�B	@4B	>�B	:xB	1�B	*�B	(>B	$�B	�B	�B		�B��B�rB�3B�[B�B�CB��B��BݘBܒB��B��B��B�9BӏB̈́B�	B�#B�lB��B�B�B��B�.B��B�XB�B�B�GB��B�qB�
B�:B�5B��B��B�B��B�B��B��B��B��B�B��B��B��B��B�7B��B��B��B�-B�B~�B|�Bz�ByXBw�Bv�Bt�BshBo�BlBl"BjeBiDBg�Bf�Bc�BdBc�BdBc�BcnBc�Bc�Bc�Bc�Bc�BeBc:B_!B]IB\BY�BX�BV�BTBR�BQ�BO�BN�BLdBL�BI�BIlBJ#BGzBCGBBuBB[B?�B;B9>B8B5�B2�B0!B-�B-�B-�B-�B,�B-CB,�B*�B'RB$�B#TB �BB�B/B�B�BBxBB�BEBB�B BBWB�B"�B$�B$�B$�B%`B%FB$�B$�B$@B"�B!HB!�B$tB&�B&LB%�B&B&fB'�B+B.IB.IB.}B/�B0�B1�B4�B6�B8B7�B8�B8�B8�B9rB@�BBABCBC-BF?BHKBJ=BJ�BK�BK)BL~BO�BS�BW�BZ�B[�B]�B]�B_Bb4BgmBnIBr-Bu?BxBxBx�B{�B�B�uB��B��B��B��B�BB�,B�B��B��B��B��B�B�B�B�B�&B��B�B�B�$B�$B�DB�KB�qB��B��B��B��B��B��B��B�4B�-B�B�B�(B�B� B�@B�B�FB�2B�SB�?B�KB�QBچBބB��B�B�B�B�B�B�B��B�B��B��B��B��B��B��B�%B�*B�B�B�B�"B�(B�HB	 OB	{B	_B	
XB	xB	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$B	%B	&B	'B	*0B	*KB	+6B	,WB	/iB	0UB	1vB	4TB	5tB	6�B	8�B	:xB	:xB	:xB	:�B	=�B	A�B	A�B	B�B	B�B	B�B	B�B	EB	F�B	J	B	NB	O(B	R B	T{B	YeB	ZQB	\]B	]~B	_�B	d�B	g�B	l�B	p�B	q�B	r�B	r�B	q�B	q�B	q�B	q�B	q�B	q�B	tB	v�B	xB	{0B	|B	B	� B	� B	�AB	�-B	�SB	�?B	�YB	�?B	�?B	�_B	�fB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�$B	�B	�*B	�6B	�)B	�/B	�/B	�iB	�AB	�GB	�GB	�MB	��B	�`B	�fB	�lB	�lB	�XB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�,B	�2B	�B	�B	�$B	�EB	�+B	�1B	�QB	�qB	�IB	�VB	�pB	�BB	�\B	�\B	�\B	�B	�hB	�nB	�nB	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�ZB	�RB	�B	�B	�B	�.B	�B	�.B
 4B
 B
'B
'B
AB
'B
'B
aB
9B
mB
tB
�B
	�B

rB
^B
^B
^B
^B
dB
dB
dB
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
 �B
!�B
!�B
#B
"�B
"�B
#�B
#�B
$B
#�B
#�B
$B
$B
$�B
%B
%�B
&2B
'8B
($B
)B
)*B
*0B
+B
+B
+6B
+6B
,B
,"B
,"B
,"B
,=B
-CB
-CB
-)B
-CB
./B
./B
.IB
.IB
/5B
0!B
0;B
0;B
0;B
1[B
1[B
1vB
2GB
2aB
2aB
3�B
3hB
4nB
5tB
5ZB
6`B
6FB
6FB
6zB
6`B
7fB
7fB
7�B
8lB
8RB
8�B
8lB
8�B
9rB
:xB
;dB
;dB
;dB
;B
;B
;B
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
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
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
M�B
N"B
NB
O�B
O�B
O�B
O�B
QB
P�B
QB
P�B
QB
RB
Q�B
RB
RB
Q�B
Q�B
Q�B
R�B
SB
SB
S&B
SB
SB
T,B
UB
T�B
UB
UB
VB
VB
VB
VB
VB
V9B
V9B
W$B
W$B
X+B
X+B
XEB
XEB
YeB
YeB
Z7B
[=B
[#B
[=B
\)B
\CB
\CB
]IB
]dB
^jB
^jB
^OB
^OB
_VB
_VB
`\B
_pB
_VB
`BB
`\B
`BB
a|B
abB
aHB
aHB
abB
a|B
bhB
bhB
bNB
bhB
cTB
cTB
cnB
cnB
dtB
dtB
dtB
dtB
dtB
ezB
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
jB
jB
jB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811150037412018111500374120181115003741202211182136512022111821365120221118213651201811160018102018111600181020181116001810  JA  ARFMdecpA19c                                                                20181105003524  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181104153555  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181104153557  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181104153558  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181104153558  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181104153558  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181104153559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181104153559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181104153559  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181104153559                      G�O�G�O�G�O�                JA  ARUP                                                                        20181104155543                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181104153638  CV  JULD            G�O�G�O�F�q                JM  ARCAJMQC2.0                                                                 20181114153741  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181114153741  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181115151810  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123651  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                