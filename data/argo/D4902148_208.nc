CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-27T18:38:03Z creation;2020-04-27T18:38:07Z conversion to V3.1;2022-11-21T05:27:11Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  M`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20200427183803  20221123114512  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_208                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�"w`�1   @�#�� @:�e+��a�dwO�M1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5��D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D¾D��D�>D�~DþD��D�>D�~DľD��D�>D�~DžD��D�>D�~DƾD��D�>D�~DǾD��D�>D�~DȾD��D�>D�~DɾD��D�>D�~DʾD��D�>D�~D˾D��D�>D�~D̾D��D�>D�~D;D��D�>D�~DξD��D�>D�~DϾD��D�>D�~DоD��D�>D�~DѾD��D�>D�~DҾD��D�>D�~DӾD��D�>D�~DԾD��D�>D�~DվD��D�>D�~D־D��D�>D�~D׾D��D�>D�~DؾD��D�>D�~DپD��D�>D�~DھD��D�>D�~D۾D��D�>D�~DܾD��D�>D�~DݾD��D�>D�~D޾D��D�>D�~D߾D��D�>D�~D�D��D�>D�z�D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�z�D�D��D�>D�~D�D��D�>D�~D�D��D�>D�HD��D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D�D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��D��D�>D�~D��HD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A�p�A���A��uA���A��uA��uA�z�A�p�A�\)A�"�A���A���A�\)A��`A��^A���A�Q�A�p�A���A�p�A��A��wA�XA��\A�ZA��A��#A�dZA�dZA�ZA�M�A�~�A�hsA���A�?}A�z�A�+A�JA��A��9A�+A���A�XA���A��7A�33A�A��#A���A��A���A��jA��A���A�&�A��A��
A���A�1'A�1'A�A�A��A���A��A�(�A���A�jA���A���A���A�A�JA�C�A�A{�;Az��Az �Ay�mAy��AyhsAx�!AxAv��AudZAuAt��AtM�At  Ar��Aq+Ao��Am�;Al �Aj�+AjJAit�Ag��Af��Ae�
AeXAc��Ab1A_|�A^ffA]XA\��A\z�A\~�A\z�A\A�A[�hA[AZVAY�AX�AW�
AV�AS�#ASXARE�APM�AN�`ALE�AK�wAK�hAKdZAJ�yAJ�9AJr�AIAI��AIl�AIO�AI/AH�AH^5AG;dAE�#AD�AC�AC?}ABr�AB-ABJAA��AA��AAC�A@��A?�
A?O�A?�A?%A>v�A<��A;/A9��A9C�A8�HA89XA7C�A5�A3�A3�A2�A2n�A1�A/XA.��A-G�A+�A*E�A)K�A(z�A'A'l�A%�^A%
=A$��A#�7A"�!A!�^A �`A M�A��A33AJA�A��AM�A�^At�AO�A�A^5A�A��AA�A�HA-AhsA�HA��AQ�A&�A�DAA+A �A�A�/A�jA1A�/AƨA
��A	�TA	dZA�A�A{A��AK�A��A\)A��A^5A1A��A|�A?}A��AM�A��A/A ��A �uA 1'@�o@�5?@�+@���@�7L@�ƨ@���@��9@�Q�@�F@�l�@�j@�r�@띲@��@�$�@�u@��@�1'@�S�@�@�{@���@ܣ�@ܓu@�z�@��@؃@�
=@��@Չ7@���@ӍP@�V@�K�@θR@Ͳ-@� �@���@���@�\)@�o@��@ư!@Ƈ+@�V@�@��/@+@�+@���@���@�33@���@�-@��@�r�@���@��y@���@���@��u@� �@���@��P@��@��T@�X@��`@��u@�  @���@�ff@���@�/@���@�j@�Q�@�1@���@��y@��@��^@��7@�G�@���@��u@�(�@�1@��F@��H@�ff@�5?@�J@���@�l�@�
=@��\@��-@�x�@�A�@�dZ@�ȴ@�M�@��^@�/@�(�@��
@�l�@�@���@�$�@�@���@�G�@�Ĝ@��F@�ȴ@���@�M�@��#@�O�@�z�@�ƨ@�o@�v�@�{@���@���@��u@�bN@�b@���@��
@���@���@��@�dZ@�S�@��y@��!@�M�@��T@���@��-@���@��`@�I�@���@��F@���@�l�@��@�v�@��@�@�x�@��`@�Ĝ@���@�z�@�A�@��
@��P@��@��@�ff@�J@��T@��T@�?}@�z�@�I�@�9X@�(�@�1@���@��@���@���@�v�@�^5@���@�p�@�/@��@���@��@��u@�z�@��@|�@+@~�y@~v�@~V@~@|��@{�@{33@z�H@z��@z�\@y��@y�7@x�9@xb@w�@w�;@w��@w�w@w�P@v��@v��@v{@u��@u�@u/@t�@t(�@s"�@r��@r^5@q�@qhs@p�@o�P@o
=@nv�@n5?@m�@m/@mV@l�@l�@lZ@l9X@l1@k��@k�@kt�@kdZ@k33@ko@k@j�@j��@jn�@j=q@i�^@i&�@h��@h��@h��@hQ�@hb@g��@gK�@g
=@f�R@f�+@e��@e?}@d�D@c��@cƨ@c��@cC�@b��@b��@b�!@b��@b��@b��@bn�@a��@a�@`Ĝ@`��@`��@`��@`�u@`  @_��@_|�@_\)@_;d@_�@^ȴ@^v�@]�@\�@\��@\z�@\�@[�m@[�@Z�!@Z^5@Z=q@Z=q@Z-@Z�@ZJ@Y��@Y�@X��@XĜ@X�9@X�9@X�u@X �@W�@W��@W��@Wl�@Vȴ@U�@U��@UO�@U�@U�@T��@Tj@S�
@S��@SS�@S"�@S@R��@Q��@QG�@P��@P��@P��@P�`@P��@P �@O|�@O+@O�@O
=@O
=@N�y@Nv�@M�@M�-@M/@L�D@Lj@Lj@LI�@L1@KdZ@J�H@J��@Jn�@J^5@J=q@I��@I��@HĜ@Hb@G�@G��@G�@G�P@G\)@GK�@G�@F��@F�y@Fȴ@F��@F�+@Fff@F5?@E��@Ep�@D�@D9X@Ct�@C33@B��@Bn�@B=q@B-@BJ@A��@A�#@AG�@@Ĝ@@bN@@ �@?�@?��@?|�@?l�@?\)@?+@?
=@>ȴ@>��@>ff@>V@>E�@>5?@>$�@=�@=�T@=�T@=��@=@=@=@=@=�@=`B@=�@<�/@<�/@<��@<��@<�@<�D@<z�@<Z@<(�@;ƨ@;��@;C�@:��@:=q@9&�@8�9@8  @7�@7�@6��@6�y@6��@65?@6@5�h@5/@4��@4��@49X@41@3�
@3S�@2�H@2�!@2��@2~�@2^5@2=q@2-@1��@1��@1X@1G�@1%@0bN@0 �@0  @/l�@/\)@/K�@/;d@/�@.ȴ@.��@.��@.�+@.ff@.V@.E�@-�h@-`B@-/@,�@,�j@,�@,�D@,j@+ƨ@+C�@+@+@*��@*��@*�\@)�@)�^@)hs@)&�@(��@(Ĝ@(�9@(�9@(�@(1'@'��@'�@'��@'K�@&ff@&@%��@%p�@%?}@%V@$�j@$��@$j@$9X@#ƨ@#��@#dZ@#C�@#o@"��@"~�@"M�@!�@!�7@!X@!7L@!7L@!7L@!&�@ ��@ Ĝ@ �u@ A�@ b@�w@|�@+@�y@�@�R@��@v�@V@E�@5?@$�@�@�-@p�@?}@�@�@�@V@�@�j@�@�D@j@9X@(�@�@�
@�@t�@S�@"�@o@�H@^5@=q@-@�@�@-@-@�@�@��@�^@x�@&�@��@��@��@�@Q�@ �@  @�@�w@��@l�@;d@�@��@v�@v�@E�@@@�h@�@�@`B@?}@�@��@�@��@j@Z@I�@�@�@��@t�@33@"�@@�H@��@=q@�@��@�7@&�@%@��@Ĝ@�u@�@A�@��@�P@K�@�y@�@�R@v�@E�@5?@5?@{@��@��@�h@�@`B@`B@O�@/@V@V@��@��@��@�@z�@Z@(�@�@�@1@dZ@o@
�H@
��@
��@
��@
��@
��@
�!@
��@
~�@
n�@
n�@
n�@
M�@
�@	�@	��@	X@	%@��@r�@1'@ �@ �@  @�@�;@��@�@�P@|�@
=@�@�R@5?@��@O�@�@�@�@j@9X@(�@��@�
@�F@��@��@��@�@t�@S�@33@@�H@�!@��@��@��@n�@M�@=q@J@�@�#@��@��@�^@hs@G�@G�@G�@&�@ ��@ �`@ �@ r�@ r�@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A���A���A�p�A���A��uA���A��uA��uA�z�A�p�A�\)A�"�A���A���A�\)A��`A��^A���A�Q�A�p�A���A�p�A��A��wA�XA��\A�ZA��A��#A�dZA�dZA�ZA�M�A�~�A�hsA���A�?}A�z�A�+A�JA��A��9A�+A���A�XA���A��7A�33A�A��#A���A��A���A��jA��A���A�&�A��A��
A���A�1'A�1'A�A�A��A���A��A�(�A���A�jA���A���A���A�A�JA�C�A�A{�;Az��Az �Ay�mAy��AyhsAx�!AxAv��AudZAuAt��AtM�At  Ar��Aq+Ao��Am�;Al �Aj�+AjJAit�Ag��Af��Ae�
AeXAc��Ab1A_|�A^ffA]XA\��A\z�A\~�A\z�A\A�A[�hA[AZVAY�AX�AW�
AV�AS�#ASXARE�APM�AN�`ALE�AK�wAK�hAKdZAJ�yAJ�9AJr�AIAI��AIl�AIO�AI/AH�AH^5AG;dAE�#AD�AC�AC?}ABr�AB-ABJAA��AA��AAC�A@��A?�
A?O�A?�A?%A>v�A<��A;/A9��A9C�A8�HA89XA7C�A5�A3�A3�A2�A2n�A1�A/XA.��A-G�A+�A*E�A)K�A(z�A'A'l�A%�^A%
=A$��A#�7A"�!A!�^A �`A M�A��A33AJA�A��AM�A�^At�AO�A�A^5A�A��AA�A�HA-AhsA�HA��AQ�A&�A�DAA+A �A�A�/A�jA1A�/AƨA
��A	�TA	dZA�A�A{A��AK�A��A\)A��A^5A1A��A|�A?}A��AM�A��A/A ��A �uA 1'@�o@�5?@�+@���@�7L@�ƨ@���@��9@�Q�@�F@�l�@�j@�r�@띲@��@�$�@�u@��@�1'@�S�@�@�{@���@ܣ�@ܓu@�z�@��@؃@�
=@��@Չ7@���@ӍP@�V@�K�@θR@Ͳ-@� �@���@���@�\)@�o@��@ư!@Ƈ+@�V@�@��/@+@�+@���@���@�33@���@�-@��@�r�@���@��y@���@���@��u@� �@���@��P@��@��T@�X@��`@��u@�  @���@�ff@���@�/@���@�j@�Q�@�1@���@��y@��@��^@��7@�G�@���@��u@�(�@�1@��F@��H@�ff@�5?@�J@���@�l�@�
=@��\@��-@�x�@�A�@�dZ@�ȴ@�M�@��^@�/@�(�@��
@�l�@�@���@�$�@�@���@�G�@�Ĝ@��F@�ȴ@���@�M�@��#@�O�@�z�@�ƨ@�o@�v�@�{@���@���@��u@�bN@�b@���@��
@���@���@��@�dZ@�S�@��y@��!@�M�@��T@���@��-@���@��`@�I�@���@��F@���@�l�@��@�v�@��@�@�x�@��`@�Ĝ@���@�z�@�A�@��
@��P@��@��@�ff@�J@��T@��T@�?}@�z�@�I�@�9X@�(�@�1@���@��@���@���@�v�@�^5@���@�p�@�/@��@���@��@��u@�z�@��@|�@+@~�y@~v�@~V@~@|��@{�@{33@z�H@z��@z�\@y��@y�7@x�9@xb@w�@w�;@w��@w�w@w�P@v��@v��@v{@u��@u�@u/@t�@t(�@s"�@r��@r^5@q�@qhs@p�@o�P@o
=@nv�@n5?@m�@m/@mV@l�@l�@lZ@l9X@l1@k��@k�@kt�@kdZ@k33@ko@k@j�@j��@jn�@j=q@i�^@i&�@h��@h��@h��@hQ�@hb@g��@gK�@g
=@f�R@f�+@e��@e?}@d�D@c��@cƨ@c��@cC�@b��@b��@b�!@b��@b��@b��@bn�@a��@a�@`Ĝ@`��@`��@`��@`�u@`  @_��@_|�@_\)@_;d@_�@^ȴ@^v�@]�@\�@\��@\z�@\�@[�m@[�@Z�!@Z^5@Z=q@Z=q@Z-@Z�@ZJ@Y��@Y�@X��@XĜ@X�9@X�9@X�u@X �@W�@W��@W��@Wl�@Vȴ@U�@U��@UO�@U�@U�@T��@Tj@S�
@S��@SS�@S"�@S@R��@Q��@QG�@P��@P��@P��@P�`@P��@P �@O|�@O+@O�@O
=@O
=@N�y@Nv�@M�@M�-@M/@L�D@Lj@Lj@LI�@L1@KdZ@J�H@J��@Jn�@J^5@J=q@I��@I��@HĜ@Hb@G�@G��@G�@G�P@G\)@GK�@G�@F��@F�y@Fȴ@F��@F�+@Fff@F5?@E��@Ep�@D�@D9X@Ct�@C33@B��@Bn�@B=q@B-@BJ@A��@A�#@AG�@@Ĝ@@bN@@ �@?�@?��@?|�@?l�@?\)@?+@?
=@>ȴ@>��@>ff@>V@>E�@>5?@>$�@=�@=�T@=�T@=��@=@=@=@=@=�@=`B@=�@<�/@<�/@<��@<��@<�@<�D@<z�@<Z@<(�@;ƨ@;��@;C�@:��@:=q@9&�@8�9@8  @7�@7�@6��@6�y@6��@65?@6@5�h@5/@4��@4��@49X@41@3�
@3S�@2�H@2�!@2��@2~�@2^5@2=q@2-@1��@1��@1X@1G�@1%@0bN@0 �@0  @/l�@/\)@/K�@/;d@/�@.ȴ@.��@.��@.�+@.ff@.V@.E�@-�h@-`B@-/@,�@,�j@,�@,�D@,j@+ƨ@+C�@+@+@*��@*��@*�\@)�@)�^@)hs@)&�@(��@(Ĝ@(�9@(�9@(�@(1'@'��@'�@'��@'K�@&ff@&@%��@%p�@%?}@%V@$�j@$��@$j@$9X@#ƨ@#��@#dZ@#C�@#o@"��@"~�@"M�@!�@!�7@!X@!7L@!7L@!7L@!&�@ ��@ Ĝ@ �u@ A�@ b@�w@|�@+@�y@�@�R@��@v�@V@E�@5?@$�@�@�-@p�@?}@�@�@�@V@�@�j@�@�D@j@9X@(�@�@�
@�@t�@S�@"�@o@�H@^5@=q@-@�@�@-@-@�@�@��@�^@x�@&�@��@��@��@�@Q�@ �@  @�@�w@��@l�@;d@�@��@v�@v�@E�@@@�h@�@�@`B@?}@�@��@�@��@j@Z@I�@�@�@��@t�@33@"�@@�H@��@=q@�@��@�7@&�@%@��@Ĝ@�u@�@A�@��@�P@K�@�y@�@�R@v�@E�@5?@5?@{@��@��@�h@�@`B@`B@O�@/@V@V@��@��@��@�@z�@Z@(�@�@�@1@dZ@o@
�H@
��@
��@
��@
��@
��@
�!@
��@
~�@
n�@
n�@
n�@
M�@
�@	�@	��@	X@	%@��@r�@1'@ �@ �@  @�@�;@��@�@�P@|�@
=@�@�R@5?@��@O�@�@�@�@j@9X@(�@��@�
@�F@��@��@��@�@t�@S�@33@@�H@�!@��@��@��@n�@M�@=q@J@�@�#@��@��@�^@hs@G�@G�@G�@&�@ ��@ �`@ �@ r�@ r�@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�/B�/B�/B�/B�/B�#B��BBBĜBĜBŢBÖBB�}B�^B��B��B��B�oB�hB�bB�PB�=B�%B�B�B�B�B�B�DB�7B{�By�B|�B~�Bn�B\)BJ�B?}B<jB>wB<jB;dB8RB+B"�B�B�BJB	7BB�B�NB��B�XB��B��B�JBx�B`BBXBK�B<jB33B%�B�B%BB
��B
��B
�B
�B
�TB
�#B
ĜB
�FB
��B
��B
�B
n�B
e`B
bNB
`BB
_;B
\)B
VB
P�B
G�B
>wB
;dB
9XB
5?B
2-B
(�B
�B
oB
%B	��B	�B	�B	�B	�HB	�#B	�B	��B	ɺB	�jB	�!B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�bB	�=B	�B	x�B	n�B	k�B	e`B	[#B	R�B	M�B	L�B	L�B	K�B	I�B	G�B	F�B	B�B	A�B	@�B	?}B	>wB	=qB	9XB	49B	.B	)�B	%�B	"�B	 �B	�B	�B	�B	�B	�B	�B	{B	hB	bB	\B	JB	B��B��B��B��B�B�B�NB�5B�)B�B�
B��B��B��BĜB�qB�FB�-B�B�B��B��B��B��B��B��B�hB�VB�JB�=B�+B�B�B� B}�B|�B{�Bz�By�Bw�Bu�Br�Bp�Bn�Bm�Bk�BiyBgmBffBdZBbNB`BB^5B\)B[#BXBW
BVBS�BQ�BN�BM�BK�BJ�BI�BH�BG�BF�BE�BD�BB�BA�B@�B@�B@�B?}B>wB=qB=qB<jB;dB;dB:^B9XB8RB7LB6FB5?B49B33B2-B2-B1'B1'B/B.B/B.B.B-B,B,B,B)�B)�B+B+B+B+B(�B(�B)�B)�B+B+B)�B)�B,B,B,B,B,B.B0!B1'B1'B1'B1'B1'B1'B0!B0!B2-B7LB:^B<jB<jB<jB=qB@�B@�BA�BB�BD�BE�BF�BG�BH�BH�BI�BK�BL�BM�BM�BN�BQ�BR�BS�BVBW
BXBXBXBYBZB\)B\)B]/B]/B^5B^5B_;B_;B`BBbNBdZBdZBdZBhsBl�Bm�Bn�Bp�Bq�Bt�Bw�Bz�B|�B~�B� B�B�%B�1B�=B�JB�\B�hB�oB�uB��B��B��B��B��B��B��B�B�B�3B�LB�XB�dB��BBÖBŢBƨBǮBȴBȴBɺBɺB��B��B��B��B��B��B��B��B�B�;B�HB�TB�TB�ZB�yB�B�B�B�B��B��B��B��B��B��B	  B	B	B		7B	JB	uB	�B	�B	�B	�B	�B	�B	�B	�B	$�B	'�B	+B	-B	.B	49B	8RB	;dB	;dB	=qB	?}B	@�B	A�B	E�B	H�B	J�B	M�B	O�B	P�B	R�B	XB	]/B	_;B	`BB	aHB	bNB	dZB	e`B	hsB	jB	k�B	k�B	k�B	k�B	l�B	n�B	p�B	r�B	u�B	v�B	w�B	x�B	z�B	~�B	�B	�B	�B	�B	�+B	�=B	�JB	�\B	�\B	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�LB	�XB	�XB	�^B	�dB	�qB	�qB	�qB	�wB	�wB	�wB	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ǮB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�;B	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB
PB
VB
hB
hB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
&�B
(�B
)�B
,B
,B
-B
-B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
2-B
33B
33B
49B
5?B
5?B
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
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
<jB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
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
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
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
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
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
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�/B�/B�/B�IB�~B�)B�:B��BªBĜBĶB��B��B�B��B��B�6B�	B�mB��B��B�hB�\B�JB��B�%B��B�YB��B��B��B�B|�BzDB}�B� Bp�B^�BL~B@�B=�B?B<�B<�B:xB,WB#�B�B�BB
�B?B�-B�FB��B�B�$B��B�vB|Ba�BZBNB>]B5�B($B�BB�B
��B
��B
�B
��B
�B
ބB
�+B
��B
�B
��B
�fB
pB
e�B
b�B
`�B
_�B
]B
W$B
RoB
H�B
?B
;�B
9�B
6B
4B
*�B
�B
�B
1B	��B	�B	�B	�qB	�B	�]B	�$B	��B	�JB	�.B	��B	�B	��B	�B	��B	��B	�BB	��B	�qB	�sB	�@B	��B	��B	�SB	{0B	o�B	mCB	g�B	]IB	U�B	N�B	MB	M6B	LdB	J=B	H1B	G_B	B�B	A�B	@�B	?�B	?B	>wB	:�B	5�B	/OB	+6B	&�B	#�B	!-B	�B	!B	5B	dB	�B	�B	B	�B	�B	�B	�B	�B	 �B��B��B��B�GB��B�B�B�B�=B��B��B��B��B�tB�}B��B�MB�!B��B��B��B��B�'B��B��B��B�BB�B�xB��B�MB��B��B~�B}VB|PB{�Bz�Bx�Bv�BtBq�Bo�Bn�Bl�BjKBg�Bg8Be�BcTBa|B_;B]�B\xBX�BW�BW?BU�BS�BPbBN�BL~BKxBJrBIlBHfBGzBF�BF%BC�BBBABABAB@ B?.B>BB>]B=B<B;�B;B:xB9XB9>B7LB5�B5ZB4�B2�B2�B1�B1�B1'B0oB/�B.�B/5B.}B./B,�B-B+�B+�B+�B+6B+6B+�B*0B*�B+B*�B+�B+�B+6B+�B-CB,�B-B-]B-�B/5B0�B1vB1[B1[B1[B1vB1�B1vB2-B4TB8�B;JB<�B<�B="B>wB@�BAUBB[BCaBE9BFBGBG�BI7BIRBJrBLJBM6BN<BNpBO�BRoBS�BT{BVmBWYBX_BXyBX�BY�BZ�B\]B\]B]�B]~B^�B^�B_pB_�B`�Bb�Bd�Bd�BezBiyBl�BnBo5BqBr|ButBxRB{JB}qB}B��B�mB��B��B��B��B��B��B��B�B�SB�CB��B�-B�4B�ZB��B��B��B��B��B��B��B��B��B��BżB��B��B��B��B�	B��B�B�B�BB�4B�B�,B�MB՛BڠB�pB�|B�B�B��B��B��B��B�B�3B��B�B�	B�*B�6B�HB	 iB	MB	�B		�B	~B	�B	B	7B	�B	�B	�B	�B	B	 'B	%B	($B	+6B	-]B	.}B	4�B	8�B	;�B	;�B	=�B	?�B	@�B	A�B	E�B	H�B	J�B	NB	O�B	Q4B	SuB	XyB	]dB	_VB	`\B	a|B	b�B	d�B	e�B	h�B	j�B	k�B	k�B	k�B	k�B	l�B	n�B	p�B	r�B	u�B	wB	xB	y>B	{JB	.B	�;B	�[B	�aB	��B	�zB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�>B	�B	�6B	�=B	�wB	�vB	��B	��B	�rB	�rB	��B	��B	�qB	�qB	�qB	��B	��B	��B	��B	��B	ªB	ðB	ĜB	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�(B	�B	�B	�4B	�B	�B	��B	�B	�2B	�B	�2B	�SB	�+B	�1B	�KB	�1B	�1B	�KB	�WB	�CB	�CB	�dB	ݘB	ߊB	�bB	�hB	�nB	�nB	�tB	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�B	�0B	�<B	�B	�.B	�B	�.B
 B
 4B
UB
GB
9B
?B
?B
?B
?B
+B
EB
KB
KB
KB
KB
	RB
	RB
	RB

rB
xB
xB
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#B
#�B
#�B
$B
%B
&LB
'RB
)*B
*KB
,"B
,WB
-)B
-)B
./B
.IB
/OB
/OB
0;B
0;B
1[B
1AB
2GB
3hB
3�B
4nB
5tB
5ZB
6zB
6zB
6`B
7fB
7fB
7fB
8lB
8�B
8lB
8�B
:xB
:xB
:�B
;dB
;B
;B
;�B
;�B
<�B
<jB
<jB
<�B
<�B
=�B
<�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
GB
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
MB
M�B
NB
M�B
NB
N�B
O�B
O�B
O�B
O�B
PB
O�B
Q B
QB
QB
RB
R B
RB
RB
SB
SB
SB
SB
TB
T,B
TB
S�B
T,B
TB
UB
U2B
UB
VB
VB
VB
VB
VB
VB
W
B
W?B
W?B
W$B
X+B
X+B
XEB
X+B
X+B
Y1B
Y1B
Z7B
ZQB
ZkB
[=B
[#B
[=B
[#B
[#B
[#B
[WB
\CB
\CB
\CB
\]B
\CB
\CB
\CB
]/B
]dB
]IB
]IB
]IB
]IB
]dB
^OB
^jB
^jB
_VB
_�B
_VB
`BB
`\B
`\B
`vB
abB
aHB
abB
a|B
abB
bhB
bhB
bhB
cnB
cnB
cTB
cnB
cnB
dZB
dtB
d�B
d�B
dZB
dtB
dtB
dtB
ezB
f�B
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
i�B
i�B
j�B
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
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
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
xB
xB
xB
y	B
y	B
x�B
y�B
zB
zB
z�B
z�B
z�B
z�B
|B
{�B
{�B
|B
{�B
|B
|B
|B
|B
|B
{�B
|B
|B
|B
|�B
}B
}B
}�B
~B
}�B
}�B
~B
~(B
~�B
~�B
~�B
B
B
.B
B
~�B
~�B
� B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005080034552020050800345520200508003455202211182142562022111821425620221118214256202005090022072020050900220720200509002207  JA  ARFMdecpA19c                                                                20200428033740  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200427183803  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200427183805  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200427183806  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200427183806  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200427183806  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200427183807  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200427183807  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200427183807  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200427183807  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200427183807  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200427183807                      G�O�G�O�G�O�                JA  ARUP                                                                        20200427185520                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200428153342  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200428153319  CV  JULD            G�O�G�O�Fȩ                JM  ARGQJMQC2.0                                                                 20200428153319  CV  JULD_LOCATION   G�O�G�O�Fȩ*                JM  ARGQJMQC2.0                                                                 20200428153319  CV  LATITUDE        G�O�G�O�A�Q�                JM  ARGQJMQC2.0                                                                 20200428153319  CV  LONGITUDE       G�O�G�O��#�#                JM  ARCAJMQC2.0                                                                 20200507153455  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200507153455  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200508152207  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124256  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114512                      G�O�G�O�G�O�                