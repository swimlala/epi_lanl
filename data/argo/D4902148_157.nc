CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-12-04T15:37:07Z creation;2018-12-04T15:37:11Z conversion to V3.1;2019-12-18T07:18:30Z update;2022-11-21T05:29:45Z update;     
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
resolution        =���   axis      Z        H  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ih   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  M<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  `X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     H  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181204153707  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_157                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @ؕ��
��1   @ؕ���-�@<o����d����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�H@/\)@|(�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C
=C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�AHD�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111|�A�dZA�dZA�hsA�ffA�jA�l�A�p�A�r�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�v�A�x�A�v�A�x�A�x�A�z�A�z�A�|�A�|�A��A��A�~�A�~�A�|�A�~�A�~�A�z�A�|�A�~�A�z�A�x�A�n�A��A���A���A���A�&�A�K�A��A���A�ffA���A��7A��A��A���A�%A�bNA��7A���A��
A���A���A��hA�VA�1'A���A�z�A��-A�I�A�v�A�z�A�33A��A��A��A���A���A���A���A�?}A��A���A��9A�t�A�/A�;dA���A���A�x�A�n�A�+A��TA��A�1'A���A��A�jA��hAG�A~ȴA}`BA{S�AyXAv�At�`At��At�Asp�Aq|�Ao�Am��Am��AmhsAm�Al�+Ak��Aj�Ah��Ag�#Af��AdȴAc�-AcAa��A^�yA\1AZ~�AY�TAX�AV�AV$�AU\)AT�AT1'AS�PAR�9AQ��AOƨAN1'AMdZAL��AK7LAH��AG�mAG"�AD��AD�AD$�AC��AB��ABI�AA�hAAK�A@ĜA>�yA=�A<��A;�-A:��A9x�A9�A7p�A6ZA5��A4��A4�uA4bA3G�A2��A2�A2M�A1�#A0��A01'A0JA/�;A/��A/dZA/"�A.�9A.=qA-�A,�`A,A�A+oA)�A'�A'�A'C�A&v�A%7LA$�\A$M�A$A#ƨA#�hA#O�A#VA"��A"��A"E�A!�A!��A!ƨA!��A!7LA �/A Al�AbNA�A��A&�A�-A�!AE�AAA�
A�A��A(�AI�A&�A�AffA�#Ap�A/Av�AbA��AXA
�jA	�AI�A�A��A�A�mA��A`BA�A�RAA��A��A�-A�PA �`A Q�A @��@��@�C�@��@��/@�p�@�|�@�$�@�7@�(�@��@�33@��@�n�@웦@�x�@�  @�{@�&�@�@�Z@��m@���@�1'@�=q@�@݁@�G�@��@��H@�%@�  @�|�@�+@֟�@Չ7@�G�@Ԭ@ӕ�@�(�@̴9@�ƨ@�C�@���@���@ǶF@ǝ�@ǅ@�\)@�"�@Ɨ�@���@�?}@���@ă@�@���@�dZ@�"�@�
=@�@�$�@��@�A�@���@�M�@��D@�9X@�ƨ@���@�M�@��T@���@�x�@��@���@��j@�j@�1@���@���@�~�@�X@�%@���@���@��D@��@�dZ@�C�@��@�ȴ@�/@�o@���@���@�=q@���@�X@���@�Q�@��@��P@�dZ@�5?@�&�@�bN@���@��@�Z@�(�@��@��@�l�@�;d@�"�@�
=@��y@�-@���@�G�@��/@��u@�Q�@�ƨ@���@�@�&�@��`@�Ĝ@�Z@���@�dZ@�K�@�
=@���@��!@�n�@�$�@��#@�X@��`@���@��m@�o@���@�=q@��T@���@�X@�7L@�&�@�V@���@�Ĝ@�1'@�@���@��@��@��@���@�-@��#@���@�p�@�G�@��@��/@��@�(�@� �@� �@��@�b@�dZ@�^5@��@��^@��@�V@��/@���@���@���@��D@�I�@��
@��@�ff@�$�@��T@���@�hs@�O�@�?}@���@�Q�@�9X@�(�@� �@��@l�@~�y@~�R@~��@~�+@~ff@~$�@}��@}p�@|�j@|Z@|1@{�@{@z�\@z~�@zn�@z^5@z^5@z=q@z�@y�@y��@y�7@yhs@yX@xbN@w�@w��@wl�@w\)@v��@v��@v{@u��@uO�@t�@t�j@t��@t9X@s��@st�@s"�@r^5@q�#@qhs@q�@q%@q%@p��@pĜ@pr�@pA�@pb@o�@o��@nȴ@n$�@m��@l��@lj@l(�@l1@kƨ@kS�@j�!@jn�@i�7@i7L@i%@h�9@h�@hb@g�P@g�P@gl�@gK�@g
=@fȴ@f��@f��@f�+@fff@f5?@e�T@e/@d9X@c��@a��@a�@`��@`1'@_�@_�P@_\)@_+@^��@^ȴ@]?}@\(�@[�
@[ƨ@[��@[�@[dZ@[33@Z�@Z��@Z~�@Z^5@ZM�@Y��@X �@W�@W�P@W\)@WK�@W;d@W+@V��@Vȴ@V�R@V�R@Vv�@V@U@U��@U�h@U�@Up�@U?}@UV@T�@T�/@T��@T�@T��@T�D@TI�@T1@S�
@S�F@S��@S�@St�@SdZ@SdZ@R�@R�!@R��@R=q@R�@RJ@Q�^@Q�7@Qx�@Qx�@QX@QG�@QG�@QG�@PĜ@P �@O�@O|�@O�@Nȴ@Nff@N5?@N5?@N$�@M�T@MO�@L�/@L��@L(�@Kƨ@K33@JM�@I�@Ihs@I&�@H�u@HA�@G��@G��@Gl�@G\)@G\)@GK�@G;d@GK�@G�@F��@FE�@F5?@F$�@F@E��@E`B@EV@D�j@DZ@C33@B��@BJ@A�7@A&�@@��@@Q�@?;d@?+@>�y@>��@>�y@>�y@>�y@>�y@>�y@>�@>ȴ@>ȴ@=`B@<�j@<��@<�D@<z�@<z�@<j@<Z@;��@;�@;S�@;"�@:�H@:��@:�\@9��@9�^@9�^@9��@9�7@9&�@8  @7l�@7+@7�@7�@6�y@6�+@6E�@6{@5�@5��@5�@4�@4�j@4��@4z�@4Z@3dZ@2��@2�\@2^5@2-@2J@1��@1�@1�@1�#@1�#@1��@1x�@0 �@/l�@.��@.��@-�@-@-�-@-��@-�@-`B@,��@,��@,�D@,z�@,Z@,9X@,(�@,(�@,�@,1@+�
@+��@+dZ@+"�@+o@*�@*�H@*�\@)�@)�7@)G�@)7L@)�@(��@(��@(�u@'�@'�@'�P@'l�@'l�@'\)@';d@'+@&�@&�+@&v�@&V@&E�@&@%�@%�@%@%�@%�@$�/@$�D@$(�@#ƨ@#��@#t�@#o@"~�@"-@"J@!�@!�^@!hs@!G�@!&�@ Ĝ@ Q�@ 1'@  �@   @   @�@�;@�w@��@\)@;d@;d@�@�y@ȴ@�R@�R@�+@$�@��@@�-@��@O�@�/@�@�D@�D@�D@z�@z�@Z@�@�@o@�@=q@��@��@��@��@�@�@�^@&�@Ĝ@Q�@ �@�;@��@��@�+@v�@v�@ff@ff@ff@ff@V@5?@$�@$�@@@/@j@�@33@��@~�@��@�@�`@��@�9@�9@�9@Ĝ@Ĝ@�9@�9@Ĝ@Ĝ@Ĝ@�9@�@r�@bN@Q�@1'@ �@�;@�;@�@�@�;@�w@��@�P@;d@ȴ@��@E�@{@@��@�h@�h@�h@�@�@Z@I�@9X@�m@t�@C�@C�@C�@33@o@
�H@
��@
��@
=q@	��@	�^@	��@	�7@	x�@	X@	�@��@��@�u@r�@bN@1'@b@  @�@�@�@�w@�w@�w@��@K�@
=@�y@�@�R@��@��@�+@�+@v�@v�@v�@ff@ff@V@E�@E�@5?@$�@$�@$�@{@�@�@�T@�T@��@O�@/@V@��@�@��@�j@�@�D@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111|�A�dZA�dZA�hsA�ffA�jA�l�A�p�A�r�A�t�A�v�A�v�A�v�A�t�A�t�A�v�A�v�A�x�A�v�A�x�A�x�A�z�A�z�A�|�A�|�A��A��A�~�A�~�A�|�A�~�A�~�A�z�A�|�A�~�A�z�A�x�A�n�A��A���A���A���A�&�A�K�A��A���A�ffA���A��7A��A��A���A�%A�bNA��7A���A��
A���A���A��hA�VA�1'A���A�z�A��-A�I�A�v�A�z�A�33A��A��A��A���A���A���A���A�?}A��A���A��9A�t�A�/A�;dA���A���A�x�A�n�A�+A��TA��A�1'A���A��A�jA��hAG�A~ȴA}`BA{S�AyXAv�At�`At��At�Asp�Aq|�Ao�Am��Am��AmhsAm�Al�+Ak��Aj�Ah��Ag�#Af��AdȴAc�-AcAa��A^�yA\1AZ~�AY�TAX�AV�AV$�AU\)AT�AT1'AS�PAR�9AQ��AOƨAN1'AMdZAL��AK7LAH��AG�mAG"�AD��AD�AD$�AC��AB��ABI�AA�hAAK�A@ĜA>�yA=�A<��A;�-A:��A9x�A9�A7p�A6ZA5��A4��A4�uA4bA3G�A2��A2�A2M�A1�#A0��A01'A0JA/�;A/��A/dZA/"�A.�9A.=qA-�A,�`A,A�A+oA)�A'�A'�A'C�A&v�A%7LA$�\A$M�A$A#ƨA#�hA#O�A#VA"��A"��A"E�A!�A!��A!ƨA!��A!7LA �/A Al�AbNA�A��A&�A�-A�!AE�AAA�
A�A��A(�AI�A&�A�AffA�#Ap�A/Av�AbA��AXA
�jA	�AI�A�A��A�A�mA��A`BA�A�RAA��A��A�-A�PA �`A Q�A @��@��@�C�@��@��/@�p�@�|�@�$�@�7@�(�@��@�33@��@�n�@웦@�x�@�  @�{@�&�@�@�Z@��m@���@�1'@�=q@�@݁@�G�@��@��H@�%@�  @�|�@�+@֟�@Չ7@�G�@Ԭ@ӕ�@�(�@̴9@�ƨ@�C�@���@���@ǶF@ǝ�@ǅ@�\)@�"�@Ɨ�@���@�?}@���@ă@�@���@�dZ@�"�@�
=@�@�$�@��@�A�@���@�M�@��D@�9X@�ƨ@���@�M�@��T@���@�x�@��@���@��j@�j@�1@���@���@�~�@�X@�%@���@���@��D@��@�dZ@�C�@��@�ȴ@�/@�o@���@���@�=q@���@�X@���@�Q�@��@��P@�dZ@�5?@�&�@�bN@���@��@�Z@�(�@��@��@�l�@�;d@�"�@�
=@��y@�-@���@�G�@��/@��u@�Q�@�ƨ@���@�@�&�@��`@�Ĝ@�Z@���@�dZ@�K�@�
=@���@��!@�n�@�$�@��#@�X@��`@���@��m@�o@���@�=q@��T@���@�X@�7L@�&�@�V@���@�Ĝ@�1'@�@���@��@��@��@���@�-@��#@���@�p�@�G�@��@��/@��@�(�@� �@� �@��@�b@�dZ@�^5@��@��^@��@�V@��/@���@���@���@��D@�I�@��
@��@�ff@�$�@��T@���@�hs@�O�@�?}@���@�Q�@�9X@�(�@� �@��@l�@~�y@~�R@~��@~�+@~ff@~$�@}��@}p�@|�j@|Z@|1@{�@{@z�\@z~�@zn�@z^5@z^5@z=q@z�@y�@y��@y�7@yhs@yX@xbN@w�@w��@wl�@w\)@v��@v��@v{@u��@uO�@t�@t�j@t��@t9X@s��@st�@s"�@r^5@q�#@qhs@q�@q%@q%@p��@pĜ@pr�@pA�@pb@o�@o��@nȴ@n$�@m��@l��@lj@l(�@l1@kƨ@kS�@j�!@jn�@i�7@i7L@i%@h�9@h�@hb@g�P@g�P@gl�@gK�@g
=@fȴ@f��@f��@f�+@fff@f5?@e�T@e/@d9X@c��@a��@a�@`��@`1'@_�@_�P@_\)@_+@^��@^ȴ@]?}@\(�@[�
@[ƨ@[��@[�@[dZ@[33@Z�@Z��@Z~�@Z^5@ZM�@Y��@X �@W�@W�P@W\)@WK�@W;d@W+@V��@Vȴ@V�R@V�R@Vv�@V@U@U��@U�h@U�@Up�@U?}@UV@T�@T�/@T��@T�@T��@T�D@TI�@T1@S�
@S�F@S��@S�@St�@SdZ@SdZ@R�@R�!@R��@R=q@R�@RJ@Q�^@Q�7@Qx�@Qx�@QX@QG�@QG�@QG�@PĜ@P �@O�@O|�@O�@Nȴ@Nff@N5?@N5?@N$�@M�T@MO�@L�/@L��@L(�@Kƨ@K33@JM�@I�@Ihs@I&�@H�u@HA�@G��@G��@Gl�@G\)@G\)@GK�@G;d@GK�@G�@F��@FE�@F5?@F$�@F@E��@E`B@EV@D�j@DZ@C33@B��@BJ@A�7@A&�@@��@@Q�@?;d@?+@>�y@>��@>�y@>�y@>�y@>�y@>�y@>�@>ȴ@>ȴ@=`B@<�j@<��@<�D@<z�@<z�@<j@<Z@;��@;�@;S�@;"�@:�H@:��@:�\@9��@9�^@9�^@9��@9�7@9&�@8  @7l�@7+@7�@7�@6�y@6�+@6E�@6{@5�@5��@5�@4�@4�j@4��@4z�@4Z@3dZ@2��@2�\@2^5@2-@2J@1��@1�@1�@1�#@1�#@1��@1x�@0 �@/l�@.��@.��@-�@-@-�-@-��@-�@-`B@,��@,��@,�D@,z�@,Z@,9X@,(�@,(�@,�@,1@+�
@+��@+dZ@+"�@+o@*�@*�H@*�\@)�@)�7@)G�@)7L@)�@(��@(��@(�u@'�@'�@'�P@'l�@'l�@'\)@';d@'+@&�@&�+@&v�@&V@&E�@&@%�@%�@%@%�@%�@$�/@$�D@$(�@#ƨ@#��@#t�@#o@"~�@"-@"J@!�@!�^@!hs@!G�@!&�@ Ĝ@ Q�@ 1'@  �@   @   @�@�;@�w@��@\)@;d@;d@�@�y@ȴ@�R@�R@�+@$�@��@@�-@��@O�@�/@�@�D@�D@�D@z�@z�@Z@�@�@o@�@=q@��@��@��@��@�@�@�^@&�@Ĝ@Q�@ �@�;@��@��@�+@v�@v�@ff@ff@ff@ff@V@5?@$�@$�@@@/@j@�@33@��@~�@��@�@�`@��@�9@�9@�9@Ĝ@Ĝ@�9@�9@Ĝ@Ĝ@Ĝ@�9@�@r�@bN@Q�@1'@ �@�;@�;@�@�@�;@�w@��@�P@;d@ȴ@��@E�@{@@��@�h@�h@�h@�@�@Z@I�@9X@�m@t�@C�@C�@C�@33@o@
�H@
��@
��@
=q@	��@	�^@	��@	�7@	x�@	X@	�@��@��@�u@r�@bN@1'@b@  @�@�@�@�w@�w@�w@��@K�@
=@�y@�@�R@��@��@�+@�+@v�@v�@v�@ff@ff@V@E�@E�@5?@$�@$�@$�@{@�@�@�T@�T@��@O�@/@V@��@�@��@�j@�@�D@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"�B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B!�B!�B �B �B!�B �B!�B"�B"�B"�B"�B"�B �BuB�ZB�PB`BBK�BF�BE�BE�BC�B=qB2-B#�B�BJBB��B�yB�BB�BŢB�XB��B��B��B��B��B�=B�Bx�BjBe`B`BB[#BP�B@�B5?B!�B�BJB1B%BB  B
��B
�B
�yB
�`B
�BB
��B
��B
�dB
�LB
�!B
��B
��B
��B
��B
�+B
�B
v�B
hsB
YB
G�B
=qB
:^B
6FB
1'B
$�B
�B
VB
JB

=B
1B
B	��B	�B	�yB	�TB	�#B	��B	ƨB	��B	�LB	��B	�{B	�7B	�B	~�B	s�B	n�B	jB	gmB	cTB	_;B	ZB	R�B	I�B	A�B	=qB	9XB	2-B	(�B	$�B	!�B	�B	�B	�B	�B	uB	bB	JB	
=B	+B	  B��B��B�B�B�fB�ZB�)B�
B��B��B��B��BɺBǮBƨBĜBB�wB�jB�jB�dB�^B�XB�RB�FB�9B�-B�B�B��B��B��B��B��B��B��B�oB�oB�hB�bB�\B�VB�VB�PB�JB�DB�=B�=B�=B�7B�+B�%B�B�B}�B{�Bz�Bw�Bt�Bq�Bp�Bn�Bk�BhsBe`B`BB\)BXBS�BO�BM�BL�BK�BJ�BI�BG�BF�BE�BC�BA�B?}B>wB>wB>wB=qB;dB8RB6FB2-B1'B1'B0!B0!B/B.B-B-B-B,B+B)�B%�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuB{B{B{B{B{B{B{BuBuBhBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B$�B)�B+B+B,B,B-B-B.B/B/B/B2-B5?B6FB6FB7LB7LB8RB9XB9XB9XB9XB=qBE�BF�BG�BG�BH�BI�BI�BK�BN�BN�BN�BR�BW
BXBcTBgmBhsBiyBjBl�Bm�Bm�Bn�Bn�Bm�Bq�Bu�Bx�Bz�B|�B}�B�B�+B�PB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�FB�^B�qB�}B��BÖBƨBǮBȴBȴBȴB��B��B�
B�#B�)B�)B�/B�;B�NB�`B�fB�mB�sB�sB�B�B�B�B�B�B�B�B��B��B��B��B	B	B	B	B	%B	%B	1B	JB	�B	�B	�B	�B	�B	 �B	!�B	!�B	$�B	+B	,B	-B	.B	/B	1'B	33B	49B	49B	5?B	5?B	6FB	8RB	9XB	<jB	>wB	?}B	A�B	D�B	E�B	F�B	F�B	F�B	F�B	G�B	G�B	H�B	I�B	J�B	J�B	J�B	P�B	T�B	VB	W
B	W
B	XB	ZB	\)B	]/B	_;B	aHB	bNB	bNB	dZB	gmB	hsB	iyB	l�B	n�B	q�B	r�B	r�B	s�B	s�B	u�B	w�B	y�B	{�B	|�B	~�B	�B	�B	�+B	�=B	�JB	�VB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�LB	�RB	�^B	�jB	�jB	�qB	�qB	�wB	�wB	ÖB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
	7B

=B

=B
PB
VB
\B
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
)�B
)�B
)�B
)�B
)�B
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
/B
2-B
33B
33B
49B
5?B
6FB
6FB
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
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
@�B
@�B
@�B
@�B
A�B
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
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
VB
VB
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
[#B
]/B
]/B
^5B
^5B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
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
cTB
cTB
cTB
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
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"�B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B �B!�B!�B �B �B!�B �B!�B"�B"�B#B#:B$B$�B�B�-B��Be�BM�BH�BG_BF�BEmB@�B5�B&�B�B�B�B��B�B�BںB��B�JB��B�`B��B�B�+B��B�Bz�BkkBfLBaHB]/BTBB�B7�B#�B�BB�B�B�B B
��B
�B
�0B
�fB
�B
�MB
�oB
�B
��B
�'B
��B
��B
��B
�sB
�fB
��B
yXB
kB
\B
IlB
>B
;0B
7�B
3�B
'RB
�B
�B
�B

�B
	7B
SB
  B	�tB	�B	�B	�IB	�HB	�B	ªB	��B	��B	�SB	�XB	��B	�B	t�B	o�B	kQB	h�B	dZB	`�B	[�B	U2B	K�B	B�B	>�B	;�B	4�B	*0B	&fB	#�B	WB	QB	eB	�B	aB	NB	B	�B		lB	�B�*B�B�B��B�mB�fBݘB�B��BѝBϫB��B�XB�1B�EBŢB��B��B��B��B��B��B��B�	B��B�%B�MB�oB��B�DB�NB�B�]B��B��B�SB��B��B��B��B��B��B��B��B��B��B��B�rB��B��B��B�_B�B�[B~�B|�B{�By�BvBraBq�Bo�Bm)Bi�Bg�BbhB^�BZBVSBP�BN�BMjBLdBK�BJXBHKBG_BF�BEBCGB?�B>�B>�B>�B>(B<�B;dB8�B3B1vB1[B0�B0�B0B.�B-�B-]B-]B,�B,B,=B)�B$&B"�B!bB �B 'B!B!B�BB~B�B�BB�B�BB�B+B�B�B�B�BB�B�B2B�B�B�BB�BFB�B�BgBSBBB�B?B�B�B�B�B
B$B+B�B+B�B�B�B�B�B	B�B�B/BIB�B�B!HB �B!�B&fB*KB+6B+QB,WB,=B-]B-wB.cB/OB/�B0B2�B5�B6�B6zB7�B7�B8�B9�B9�B9�B:�B>�BE�BF�BHBHBIBJ=BJ=BLJBO(BOBBO�BS�BW�BY�Bc�Bg�Bh�Bi�Bj�Bl�Bm�Bm�Bn�Bn�Bn/Br-Bv+By$B{0B}VB~�B��B��B��B��B��B��B�+B��B��B�B��B�&B�&B�,B�8B�_B�wB��B��B��B��B��B��B��B��B��B��B��B��B�B�DBϫB�?B�WB�CB�xB�~BߤB�B�B�B�B�B��B��B��B��B��B��B��B�AB�nB�B�B�"B�HB	[B	3B	3B	mB	tB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"B	%FB	+6B	,"B	-CB	.IB	/OB	1[B	3MB	4TB	4TB	5ZB	5tB	6zB	8�B	9�B	<�B	>�B	?�B	A�B	D�B	E�B	F�B	F�B	F�B	F�B	G�B	G�B	H�B	I�B	J�B	J�B	K)B	QB	UB	VB	W$B	WYB	XEB	ZkB	\xB	]dB	_�B	abB	b�B	b�B	d�B	g�B	h�B	i�B	l�B	n�B	q�B	r�B	r�B	s�B	s�B	u�B	w�B	y�B	|B	}<B	HB	�aB	�SB	�zB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�2B	�_B	�WB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�<B	�bB	�B	�9B	�$B	�+B	�+B	�+B	�EB	�1B	�1B	�1B	�QB	�QB	�CB	�dB	�IB	�IB	�IB	�OB	�OB	�VB	�VB	�\B	�\B	�\B	�vB	�\B	�bB	�B	�hB	�nB	�B	�B	�nB	�tB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�DB	�0B	�<B	�BB	�B
 4B
'B
[B
-B
3B
B
B
3B
B
3B
3B
SB
?B
+B
EB
EB
EB
_B
KB
	lB

rB

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
�B
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
B
�B
�B
�B
�B
�B
!-B
#B
$�B
$�B
$�B
%B
%�B
%�B
'B
'B
'B
($B
($B
*B
*B
*B
*B
*eB
,=B
-)B
-CB
./B
./B
./B
.B
/B
/5B
/B
/5B
/OB
/�B
2aB
3hB
3hB
4nB
5tB
6FB
6zB
6`B
6`B
6zB
6zB
7fB
7fB
7�B
7�B
8lB
8RB
8lB
8lB
8�B
8�B
9rB
9rB
:^B
:�B
:xB
:�B
:�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
@�B
@�B
@�B
@�B
A�B
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
C�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
NB
NB
N�B
N�B
N�B
N�B
OB
PB
PB
Q B
P�B
P�B
Q B
Q B
QB
QB
R B
SB
S&B
TB
TB
S�B
S�B
S�B
S�B
TB
T,B
U2B
V9B
VB
W?B
WsB
X+B
Y1B
Y1B
YB
YB
Y1B
YB
YB
YB
YKB
Y1B
YB
Y1B
YKB
ZkB
ZkB
[qB
]IB
]dB
^jB
^�B
_�B
`vB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
abB
a|B
abB
aHB
a|B
bNB
bhB
bhB
bhB
bNB
bhB
bNB
bhB
bhB
cnB
cnB
c�B
dtB
dtB
ezB
ezB
e�B
f�B
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
i�B
iyB
iyB
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
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
m�B
m�B
m�B
n�B
n�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<]/<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812150036372018121500363720181215003637202211182137132022111821371320221118213713201812160020522018121600205220181216002052  JA  ARFMdecpA19c                                                                20181205003624  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181204153707  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181204153709  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181204153709  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181204153710  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181204153710  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181204153710  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181204153710  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181204153710  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181204153711                      G�O�G�O�G�O�                JA  ARUP                                                                        20181204155508                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181204153632  CV  JULD            G�O�G�O�Fĭ                JM  ARCAJMQC2.0                                                                 20181214153637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181214153637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181215152052  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123713  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                