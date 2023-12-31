CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:14:37Z creation;2022-06-04T19:14:37Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604191437  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�ܱ�o��1   @�ܲ>�t�@/Ƨ�d	��l�D1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�33A   A   A@  A`  A���A�33A�  A�  A�  A�  A���A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  By��B�33B�ffB�  B�  B�  B�  B�ffB���B�33B�ffB�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�33B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C033C2  C4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\33C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�fC��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@l(�@�G�@�{A
=A;
=A[
=A|��A��RA��A��A��AͅA�Q�A�A��RBBBBB&B.B6B>BFBNBVB^BfBnBx\)B(�B�ǮB�aHB�aHB�aHB�aHB�ǮB�.B��{B�ǮB�aHB�aHB�aHB�aHB���B�aHB�aHB�aHB�aHB�aHB�aHB�aHBה{B�ǮBޔ{B�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C�
C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5�
C7�
C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw�
Cy��C{��C}��C�
C�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)De�D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxl)Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��GD�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�vDǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�6D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�6D�yGDնD��D�6D�vDֶD��D�6D�vD׶D��D�6D�vDضD��D�6D�vDٶD��D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�vDݶD��D�6D�vD޶D��D�6D�vD߶D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�/�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~A�A䎊A䎊A䎿A䎿A��A䑝A�A�@A�uA�FA䔯A��A��A�=A䫟A䰊A�-A��A��A��A��A�<A��gA�ɺA���A��A���A�JA�(�A��cAܠ'A�5tAٞA��NA���A��`Aє�A�"�A��JAɋ�A�N�A�(�A�)_AÞ�A�O�A��A���A�/�A���A�D3A�W�A��6A�E9A��A��A�-�A���A�j�A�A�,A�ǮA�3hA�.�A��:A���A�d�A���A���A���A�C�A��{A��fA�xA�;0A���A���A�Z�A���A���A��A��A��"A���A�,A�i�A��NA�7A}��Aw�HAo��Aj�Af�Aa2�A_J�AYm]AW �AR��AQ�uAP��AN҉AK�]AEV�AAa|A?�A>��A=��A<�<A<y�A<hsA<?}A;��A:�A7�A5�jA4�|A4��A4!�A3^5A2�A/��A.��A+&�A(g8A)$tA(��A'�]A&'RA#w2A"oiA"uA!zA!&A!�A!/�A!eA �AA� A��A~(A�An�AHAA�A|AXyA�A�A��Al�A�AF�A��A�'A\�A��AVA�A��AںA�=Ap;AU�AOA�A��A&A��A��AW?A�aAC-A��A*�AMjA͟AQ�A�CA��A�A
��A
U�A
�A	�"A	��A	A�A��A-�AA��A6�A�A�xAC-A�A�A��A�A�KA�'A\�AOA�'A&�A��A�:AzxA4�A �A ��A !�@�J�@�y>@�5?@��#@��{@��2@��@�@�IR@��@��@�Z@�خ@���@���@�j@���@��k@��	@���@�O@�,�@���@�:@�@�?�@�@��@�Ɇ@�ԕ@�33@���@��H@��?@�C@�ں@�.@�&@跀@褩@�`�@�0@�/�@�S@�V�@�)_@�A�@��@��@�=q@�^@��@�~�@��@�[�@߫�@�W?@�@ފr@�`�@�V@�B[@��g@�@ܟ�@۴�@�Vm@ګ6@�/�@��W@�v`@�C@إz@�3�@�
=@��@���@�Z�@��@Ӭq@�e�@���@�@Ѱ�@�k�@���@�2�@Ϙ�@ίO@�V�@� �@Ͳ�@��@̓u@ˮ@���@ʇ�@�1�@ɼ�@�m]@���@�O@Ǧ�@�a@�)_@ƹ$@���@ſH@ŖS@�!-@�oi@��z@�Y@1@�g8@�!�@��A@�;d@���@��@���@�+@��2@���@�.�@�S�@�ں@�h�@�@��W@��@�Q�@��X@�\�@��@�@�P�@��@���@�p;@�GE@�u@��a@�e,@��m@�5?@�,=@�4@���@��f@�\�@�6@��A@�J�@���@��e@�?�@�"h@��+@��g@�l�@�O�@�&�@��E@�� @�M@��z@���@�o�@�J#@��}@�%�@��P@�\�@�X@�Y@���@�v�@�$@��{@�C@�Ĝ@��4@�Ov@�@�(@�j@�5?@�u@���@�\)@� i@��@��[@��R@��@��@�*0@��y@�D�@���@��N@���@���@��"@�L�@�Ɇ@��e@�v�@�u@���@��@@�@��K@�� @�oi@�e@��Q@��V@�%F@���@��@��R@�7�@��
@��~@�/�@��@�=q@��0@�W?@�%F@��@�ȴ@�~(@�l"@�l"@�!�@���@�iD@��@���@���@��@��@��[@��V@��"@�c@�l�@�G�@�/@���@�z�@�*�@��@��7@�#�@�֡@��I@�R�@��@���@�<6@�'�@��@���@��]@���@��\@�_�@�-�@��@��@���@���@�l�@�?}@�
=@��E@��}@��@��@�=q@�	@�_@��@��@��@�5�@��9@��@�_�@�(�@�˒@�}�@�5�@���@�q@�H@��&@���@�iD@�\�@�T�@�F@�Y@��p@��\@�c @�.�@�	�@��9@�f�@�;d@���@�i�@�1'@�ԕ@��X@�iD@�
=@���@���@�R�@�#:@��W@��f@�C�@��@�ȴ@��}@���@�_@��@�6z@�;@�ȴ@��.@�bN@��@���@�[W@�+�@��@���@�p;@�-�@�@�@~��@~+k@}hs@} \@}%@|��@|C-@{g�@{�@z�@z��@z~�@zh
@y��@y�@x��@x"h@w8@v{@u@uY�@u�@t�)@t��@t:�@s��@s"�@r�<@r&�@q��@p�$@pH@o�;@oMj@n�@ns�@n@m&�@l�/@l��@l��@l�Y@le�@l�@kx@j҉@j��@jR�@j
�@i�@i��@iQ�@h�)@hh�@h�@gv`@g)_@go@fߤ@f�1@fv�@fkQ@e��@e�z@e�=@eu�@d�P@dbN@d"h@c��@ciD@c�@b��@bh
@a�7@a%@`c�@_�Q@_H�@_Y@^�1@^z@^Ta@]��@]�@\1'@[�0@[��@Z�@Zd�@Y��@Y;@X��@X>B@W�@W�P@V�h@VR�@U��@UX@U@@T�@Ty>@T�@S�[@S�{@R�8@Rl�@R;�@Q�Z@Q�3@Q��@Q�@P��@P��@P7�@O��@O�g@O�@O4�@N�8@N��@N	@M�Z@M�z@Ms�@M2a@L��@L<�@K�}@KS�@J�@J�x@J+k@I��@IA @H��@H*�@G�P@F��@F�+@F�@E�'@Ec�@EIR@E�@D�`@D�4@DC-@C��@C�4@C.I@B��@B�F@B��@B}V@BE�@A�@Ac�@A/@A	l@@��@@V�@@,=@?خ@?;d@>҉@>�@=Vm@=!�@=q@<�I@<S�@<-�@<M@;�0@;\)@;F�@;)_@:��@:��@:�F@:5?@9�@9�H@9�h@9|@9G�@8�@8ی@8�z@8c�@89X@7�m@7�@74�@6��@6L0@5��@5�C@58�@4��@4��@4 �@3��@3�&@3��@3�f@3e�@3J#@3�@2�2@2�@2xl@2V@2B[@2@1�S@1e,@1%F@0��@0��@0��@0h�@0�@/�0@/~�@/e�@/S�@/�@.��@.�s@.��@.��@.\�@.;�@.�@.u@-��@-��@-�-@-|@-f�@-:�@-@,u�@,!@+��@+n/@+=@+'�@*��@*Q@*&�@*{@)��@)��@)�j@)ϫ@)��@)x�@)5�@)V@(��@(9X@'�@'��@'a@&�@&�}@&�+@&Z�@&!�@%�D@%��@%�^@%X@%*0@%�@%�@$��@$��@$�@$��@$D�@$G@#��@#H�@"�]@"l�@"u@!��@!u�@!N<@!:�@! \@ ��@ ��@ �u@ ~(@ Q�@ ,=@�@ݘ@�6@�w@o�@'�@
=@�@�h@R�@1�@@�j@�z@�@�=@[W@#�@�@�@�@�?@��@u�@(�@��@��@�w@�@�{@x@1�@�M@�'@��@�\@V@�@��@��@Vm@@�5@��@�@�@��@�@e�@  @خ@��@~�@Mj@!-@�M@��@}V@l�@Ta@0U@@�o@�j@�z@�C@��@��@zx@o @S&@Dg@(�@�E@�j@��@��@h�@7�@1@�;@�@@�k@iD@!-@@��@��@��@s�@Ov@	@�@�@��@@��@��@|@k�@IR@�@�K@�U@�u@A�@�@ݘ@�K@|�@.I@�"@�M@�<@s�@Q@C�@�@�>@�z@��@Y�@ \@�@��@��@z�@U2@1'@M@��@��@��@X�@8@
ȴ@
xl@
h
@
ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~A�A䎊A䎊A䎿A䎿A��A䑝A�A�@A�uA�FA䔯A��A��A�=A䫟A䰊A�-A��A��A��A��A�<A��gA�ɺA���A��A���A�JA�(�A��cAܠ'A�5tAٞA��NA���A��`Aє�A�"�A��JAɋ�A�N�A�(�A�)_AÞ�A�O�A��A���A�/�A���A�D3A�W�A��6A�E9A��A��A�-�A���A�j�A�A�,A�ǮA�3hA�.�A��:A���A�d�A���A���A���A�C�A��{A��fA�xA�;0A���A���A�Z�A���A���A��A��A��"A���A�,A�i�A��NA�7A}��Aw�HAo��Aj�Af�Aa2�A_J�AYm]AW �AR��AQ�uAP��AN҉AK�]AEV�AAa|A?�A>��A=��A<�<A<y�A<hsA<?}A;��A:�A7�A5�jA4�|A4��A4!�A3^5A2�A/��A.��A+&�A(g8A)$tA(��A'�]A&'RA#w2A"oiA"uA!zA!&A!�A!/�A!eA �AA� A��A~(A�An�AHAA�A|AXyA�A�A��Al�A�AF�A��A�'A\�A��AVA�A��AںA�=Ap;AU�AOA�A��A&A��A��AW?A�aAC-A��A*�AMjA͟AQ�A�CA��A�A
��A
U�A
�A	�"A	��A	A�A��A-�AA��A6�A�A�xAC-A�A�A��A�A�KA�'A\�AOA�'A&�A��A�:AzxA4�A �A ��A !�@�J�@�y>@�5?@��#@��{@��2@��@�@�IR@��@��@�Z@�خ@���@���@�j@���@��k@��	@���@�O@�,�@���@�:@�@�?�@�@��@�Ɇ@�ԕ@�33@���@��H@��?@�C@�ں@�.@�&@跀@褩@�`�@�0@�/�@�S@�V�@�)_@�A�@��@��@�=q@�^@��@�~�@��@�[�@߫�@�W?@�@ފr@�`�@�V@�B[@��g@�@ܟ�@۴�@�Vm@ګ6@�/�@��W@�v`@�C@إz@�3�@�
=@��@���@�Z�@��@Ӭq@�e�@���@�@Ѱ�@�k�@���@�2�@Ϙ�@ίO@�V�@� �@Ͳ�@��@̓u@ˮ@���@ʇ�@�1�@ɼ�@�m]@���@�O@Ǧ�@�a@�)_@ƹ$@���@ſH@ŖS@�!-@�oi@��z@�Y@1@�g8@�!�@��A@�;d@���@��@���@�+@��2@���@�.�@�S�@�ں@�h�@�@��W@��@�Q�@��X@�\�@��@�@�P�@��@���@�p;@�GE@�u@��a@�e,@��m@�5?@�,=@�4@���@��f@�\�@�6@��A@�J�@���@��e@�?�@�"h@��+@��g@�l�@�O�@�&�@��E@�� @�M@��z@���@�o�@�J#@��}@�%�@��P@�\�@�X@�Y@���@�v�@�$@��{@�C@�Ĝ@��4@�Ov@�@�(@�j@�5?@�u@���@�\)@� i@��@��[@��R@��@��@�*0@��y@�D�@���@��N@���@���@��"@�L�@�Ɇ@��e@�v�@�u@���@��@@�@��K@�� @�oi@�e@��Q@��V@�%F@���@��@��R@�7�@��
@��~@�/�@��@�=q@��0@�W?@�%F@��@�ȴ@�~(@�l"@�l"@�!�@���@�iD@��@���@���@��@��@��[@��V@��"@�c@�l�@�G�@�/@���@�z�@�*�@��@��7@�#�@�֡@��I@�R�@��@���@�<6@�'�@��@���@��]@���@��\@�_�@�-�@��@��@���@���@�l�@�?}@�
=@��E@��}@��@��@�=q@�	@�_@��@��@��@�5�@��9@��@�_�@�(�@�˒@�}�@�5�@���@�q@�H@��&@���@�iD@�\�@�T�@�F@�Y@��p@��\@�c @�.�@�	�@��9@�f�@�;d@���@�i�@�1'@�ԕ@��X@�iD@�
=@���@���@�R�@�#:@��W@��f@�C�@��@�ȴ@��}@���@�_@��@�6z@�;@�ȴ@��.@�bN@��@���@�[W@�+�@��@���@�p;@�-�@�@�@~��@~+k@}hs@} \@}%@|��@|C-@{g�@{�@z�@z��@z~�@zh
@y��@y�@x��@x"h@w8@v{@u@uY�@u�@t�)@t��@t:�@s��@s"�@r�<@r&�@q��@p�$@pH@o�;@oMj@n�@ns�@n@m&�@l�/@l��@l��@l�Y@le�@l�@kx@j҉@j��@jR�@j
�@i�@i��@iQ�@h�)@hh�@h�@gv`@g)_@go@fߤ@f�1@fv�@fkQ@e��@e�z@e�=@eu�@d�P@dbN@d"h@c��@ciD@c�@b��@bh
@a�7@a%@`c�@_�Q@_H�@_Y@^�1@^z@^Ta@]��@]�@\1'@[�0@[��@Z�@Zd�@Y��@Y;@X��@X>B@W�@W�P@V�h@VR�@U��@UX@U@@T�@Ty>@T�@S�[@S�{@R�8@Rl�@R;�@Q�Z@Q�3@Q��@Q�@P��@P��@P7�@O��@O�g@O�@O4�@N�8@N��@N	@M�Z@M�z@Ms�@M2a@L��@L<�@K�}@KS�@J�@J�x@J+k@I��@IA @H��@H*�@G�P@F��@F�+@F�@E�'@Ec�@EIR@E�@D�`@D�4@DC-@C��@C�4@C.I@B��@B�F@B��@B}V@BE�@A�@Ac�@A/@A	l@@��@@V�@@,=@?خ@?;d@>҉@>�@=Vm@=!�@=q@<�I@<S�@<-�@<M@;�0@;\)@;F�@;)_@:��@:��@:�F@:5?@9�@9�H@9�h@9|@9G�@8�@8ی@8�z@8c�@89X@7�m@7�@74�@6��@6L0@5��@5�C@58�@4��@4��@4 �@3��@3�&@3��@3�f@3e�@3J#@3�@2�2@2�@2xl@2V@2B[@2@1�S@1e,@1%F@0��@0��@0��@0h�@0�@/�0@/~�@/e�@/S�@/�@.��@.�s@.��@.��@.\�@.;�@.�@.u@-��@-��@-�-@-|@-f�@-:�@-@,u�@,!@+��@+n/@+=@+'�@*��@*Q@*&�@*{@)��@)��@)�j@)ϫ@)��@)x�@)5�@)V@(��@(9X@'�@'��@'a@&�@&�}@&�+@&Z�@&!�@%�D@%��@%�^@%X@%*0@%�@%�@$��@$��@$�@$��@$D�@$G@#��@#H�@"�]@"l�@"u@!��@!u�@!N<@!:�@! \@ ��@ ��@ �u@ ~(@ Q�@ ,=@�@ݘ@�6@�w@o�@'�@
=@�@�h@R�@1�@@�j@�z@�@�=@[W@#�@�@�@�@�?@��@u�@(�@��@��@�w@�@�{@x@1�@�M@�'@��@�\@V@�@��@��@Vm@@�5@��@�@�@��@�@e�@  @خ@��@~�@Mj@!-@�M@��@}V@l�@Ta@0U@@�o@�j@�z@�C@��@��@zx@o @S&@Dg@(�@�E@�j@��@��@h�@7�@1@�;@�@@�k@iD@!-@@��@��@��@s�@Ov@	@�@�@��@@��@��@|@k�@IR@�@�K@�U@�u@A�@�@ݘ@�K@|�@.I@�"@�M@�<@s�@Q@C�@�@�>@�z@��@Y�@ \@�@��@��@z�@U2@1'@M@��@��@��@X�@8@
ȴ@
xl@
h
@
ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	yB	�B	dB	dB	�B	CB	�B	pB	!|B	%�B	'8B	(�B	I�B	]B	kQB	jB	fLB	U�B	O�B	R�B	c�B	�5B	�mB
jB

=B
kkB
�2B
s�B
��B
�HB
ѷB
�DB!bBO�BQhBuZB�+B��B�B�'B��B�fB��B��B�FB��B��B�*B��B�+BvFBRoB/iB"NB
�B
�B
��B
j0B
e,B
^�B
`�B
t�B
�B
�rB
�ZB
��B
�B
tB
n�B
g�B
Q�B
0�B
3B	�B	��B	�VB	b�B	Q�B	AB	&�B	1B	�B�}B�5B�B�B�kB�B�AB�fB��B�TB��B��B��B��B�nB�TB��B��B�<B�]B�B�4B�AB�BƎB��B��B�B	�B	/B	.}B	,�B	!�B	�B	&B	'B	*KB	-)B	=VB	C�B	C�B	@�B	?�B	?�B	E�B	R�B	\B	a�B	d�B	�B	��B	�(B	��B	�[B	��B	��B	�}B	��B	��B	��B	�YB	�IB	�/B	�xB	�B	��B	�FB	�B	��B	��B	�6B	�B	�B	��B	�B	��B	�jB	��B	�BB	��B	|�B	}�B	��B	�yB	�bB	��B	�qB	��B	�FB	��B	�>B	�B	�B	�<B	�qB	�(B	�wB	�B	��B	�]B	��B	��B	��B	��B	��B	�BB	��B	��B	�VB	�.B	��B	�BB	�wB	�]B	�"B	��B	��B	�]B	��B	��B	��B	��B	ƎB	ȚB	�fB	ǮB	��B	��B	��B	�+B	�KB	��B	��B	�B	ȚB	�XB	�~B	�)B	�^B	��B	̘B	�dB	�PB	��B	�pB	�"B	�B	͟B	�DB	�=B	��B	��B	�vB	�.B	�:B	��B	ӏB	өB	�{B	ԯB	�B	өB	��B	� B	�B	�B	յB	��B	�gB	��B	��B	�gB	�MB	�2B	�MB	�B	՛B	ՁB	�2B	յB	��B	յB	ּB	֡B	�$B	��B	��B	�$B	��B	��B	֡B	�YB	��B	�B	רB	��B	�yB	ؓB	�B	ٚB	ٴB	ٴB	�QB	ڠB	�WB	�]B	ܒB	��B	�dB	ݘB	�~B	�B	ޞB	�;B	�VB	�B	޸B	�pB	��B	�bB	�HB	�bB	�bB	�4B	�B	�|B	�NB	�B	�TB	�B	�B	��B	�B	��B	�B	�B	��B	�B	�$B	�>B	�B	��B	�B	�=B	�B	��B	��B	��B	�CB	��B	�}B	�B	�OB	�B	�!B	�!B	��B	�B	��B	�B	�!B	�AB	��B	��B	��B	�|B	��B	�B	�B	��B	�B	�B	�B	�?B	�?B	�tB	�ZB	�FB	�+B	�zB	�LB	��B	��B	��B	�RB	��B	��B	��B	�rB	�XB	��B	��B	��B	��B	�B	��B	�PB	�B	��B	�6B	��B	��B	�VB	��B	��B	�B	�BB	��B	�BB	�BB	�(B	�B	��B	��B	�.B	��B
 B
�B
�B
�B
oB
B
�B
aB
GB
aB
�B
�B
3B
B
B
mB
�B
�B
�B
B
�B
�B
�B
�B
	B
	�B
	�B

�B

=B

XB

�B
)B
xB
�B
�B
JB
�B
dB
�B
�B
B
�B
B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
4B
�B
oB
oB
�B
@B
�B
�B
,B
,B
aB
FB
�B
�B
�B
B
MB
�B
�B
�B
�B
mB
�B
�B
�B
?B
sB
�B
yB
�B
�B
�B
�B
�B
�B
kB
QB
QB
�B
�B
#B
�B
B
)B
B
�B
�B
�B
�B
/B
B
~B
�B
B
�B
�B
�B
;B
 B
 'B
 �B
!�B
"B
"�B
"�B
#:B
#�B
#�B
$tB
%FB
%�B
%�B
&�B
&�B
&�B
'RB
'B
'B
'B
(�B
(�B
)*B
)�B
*B
*KB
*�B
+�B
,qB
,�B
-CB
-�B
.B
.�B
.�B
/ B
/5B
/iB
/�B
/�B
/�B
0oB
1�B
2aB
2GB
2GB
2�B
2�B
2�B
3hB
3�B
3hB
3�B
4B
3�B
4B
4�B
4nB
4nB
4nB
4�B
4�B
4�B
4�B
5ZB
5�B
6`B
6�B
6�B
72B
7fB
7�B
88B
8�B
8�B
9�B
9�B
9�B
9�B
9rB
9�B
9�B
9�B
:^B
:xB
:�B
:�B
:�B
;0B
;dB
;�B
<6B
<PB
<jB
<jB
<�B
<�B
<�B
=<B
=<B
=VB
=VB
=qB
=�B
=�B
>B
>BB
>�B
>]B
>�B
?.B
?HB
?HB
?�B
@OB
@�B
@iB
@�B
A�B
B'B
C-B
CGB
C�B
DB
DB
D3B
E�B
E�B
F%B
F?B
F�B
F�B
F�B
G+B
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
J	B
I�B
J#B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KxB
K�B
LB
LJB
L0B
LJB
L�B
L�B
M6B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
OBB
O�B
O�B
PHB
P�B
QB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
S@B
S[B
S[B
S@B
S[B
SuB
S�B
S�B
S�B
TFB
T�B
T�B
T�B
U�B
U�B
VB
V�B
V�B
V�B
WYB
WsB
WYB
WsB
W�B
W�B
W�B
XB
XEB
XEB
X_B
X�B
X�B
Y1B
YKB
YB
X�B
X�B
Y1B
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
[#B
[qB
[�B
\CB
\�B
\xB
\�B
\�B
\�B
\�B
]B
]/B
]IB
]dB
]~B
]�B
]�B
]�B
]�B
^B
^jB
^OB
^�B
^�B
^�B
^�B
_B
_!B
_pB
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`vB
`�B
`�B
aB
aB
aB
aB
aB
a-B
aHB
a�B
a�B
b4B
b�B
b�B
b�B
c B
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
d&B
d�B
d�B
d�B
eB
ezB
e�B
fB
f2B
f2B
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
gRB
gRB
g�B
g�B
g�B
h>B
hsB
h�B
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jKB
jB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kkB
kkB
k�B
k�B
lB
l"B
l=B
lWB
lqB
l�B
l�B
l�B
mB
m)B
mCB
mwB
mwB
m]B
m�B
m�B
m�B
m�B
nB
ncB
n}B
ncB
n�B
o B
oB
o B
o5B
oiB
p!B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qvB
qAB
q[B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
t�B
uB
u%B
u?B
utB
uZB
u�B
u�B
u�B
v`B
vzB
vzB
v�B
v�B
v�B
w2B
w2B
w2B
wLB
wfB
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�B
y	B
y	B
y>B
y$B
yrB
y�B
zB
zB
zxB
z�B
z�B
z�B
z�B
{B
{B
{JB
{�B
{�B
|B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}VB
}VB
}�B
}�B
~(B
~(B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	yB	�B	dB	dB	�B	CB	�B	pB	!|B	%�B	'8B	(�B	I�B	]B	kQB	jB	fLB	U�B	O�B	R�B	c�B	�5B	�mB
jB

=B
kkB
�2B
s�B
��B
�HB
ѷB
�DB!bBO�BQhBuZB�+B��B�B�'B��B�fB��B��B�FB��B��B�*B��B�+BvFBRoB/iB"NB
�B
�B
��B
j0B
e,B
^�B
`�B
t�B
�B
�rB
�ZB
��B
�B
tB
n�B
g�B
Q�B
0�B
3B	�B	��B	�VB	b�B	Q�B	AB	&�B	1B	�B�}B�5B�B�B�kB�B�AB�fB��B�TB��B��B��B��B�nB�TB��B��B�<B�]B�B�4B�AB�BƎB��B��B�B	�B	/B	.}B	,�B	!�B	�B	&B	'B	*KB	-)B	=VB	C�B	C�B	@�B	?�B	?�B	E�B	R�B	\B	a�B	d�B	�B	��B	�(B	��B	�[B	��B	��B	�}B	��B	��B	��B	�YB	�IB	�/B	�xB	�B	��B	�FB	�B	��B	��B	�6B	�B	�B	��B	�B	��B	�jB	��B	�BB	��B	|�B	}�B	��B	�yB	�bB	��B	�qB	��B	�FB	��B	�>B	�B	�B	�<B	�qB	�(B	�wB	�B	��B	�]B	��B	��B	��B	��B	��B	�BB	��B	��B	�VB	�.B	��B	�BB	�wB	�]B	�"B	��B	��B	�]B	��B	��B	��B	��B	ƎB	ȚB	�fB	ǮB	��B	��B	��B	�+B	�KB	��B	��B	�B	ȚB	�XB	�~B	�)B	�^B	��B	̘B	�dB	�PB	��B	�pB	�"B	�B	͟B	�DB	�=B	��B	��B	�vB	�.B	�:B	��B	ӏB	өB	�{B	ԯB	�B	өB	��B	� B	�B	�B	յB	��B	�gB	��B	��B	�gB	�MB	�2B	�MB	�B	՛B	ՁB	�2B	յB	��B	յB	ּB	֡B	�$B	��B	��B	�$B	��B	��B	֡B	�YB	��B	�B	רB	��B	�yB	ؓB	�B	ٚB	ٴB	ٴB	�QB	ڠB	�WB	�]B	ܒB	��B	�dB	ݘB	�~B	�B	ޞB	�;B	�VB	�B	޸B	�pB	��B	�bB	�HB	�bB	�bB	�4B	�B	�|B	�NB	�B	�TB	�B	�B	��B	�B	��B	�B	�B	��B	�B	�$B	�>B	�B	��B	�B	�=B	�B	��B	��B	��B	�CB	��B	�}B	�B	�OB	�B	�!B	�!B	��B	�B	��B	�B	�!B	�AB	��B	��B	��B	�|B	��B	�B	�B	��B	�B	�B	�B	�?B	�?B	�tB	�ZB	�FB	�+B	�zB	�LB	��B	��B	��B	�RB	��B	��B	��B	�rB	�XB	��B	��B	��B	��B	�B	��B	�PB	�B	��B	�6B	��B	��B	�VB	��B	��B	�B	�BB	��B	�BB	�BB	�(B	�B	��B	��B	�.B	��B
 B
�B
�B
�B
oB
B
�B
aB
GB
aB
�B
�B
3B
B
B
mB
�B
�B
�B
B
�B
�B
�B
�B
	B
	�B
	�B

�B

=B

XB

�B
)B
xB
�B
�B
JB
�B
dB
�B
�B
B
�B
B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
4B
�B
oB
oB
�B
@B
�B
�B
,B
,B
aB
FB
�B
�B
�B
B
MB
�B
�B
�B
�B
mB
�B
�B
�B
?B
sB
�B
yB
�B
�B
�B
�B
�B
�B
kB
QB
QB
�B
�B
#B
�B
B
)B
B
�B
�B
�B
�B
/B
B
~B
�B
B
�B
�B
�B
;B
 B
 'B
 �B
!�B
"B
"�B
"�B
#:B
#�B
#�B
$tB
%FB
%�B
%�B
&�B
&�B
&�B
'RB
'B
'B
'B
(�B
(�B
)*B
)�B
*B
*KB
*�B
+�B
,qB
,�B
-CB
-�B
.B
.�B
.�B
/ B
/5B
/iB
/�B
/�B
/�B
0oB
1�B
2aB
2GB
2GB
2�B
2�B
2�B
3hB
3�B
3hB
3�B
4B
3�B
4B
4�B
4nB
4nB
4nB
4�B
4�B
4�B
4�B
5ZB
5�B
6`B
6�B
6�B
72B
7fB
7�B
88B
8�B
8�B
9�B
9�B
9�B
9�B
9rB
9�B
9�B
9�B
:^B
:xB
:�B
:�B
:�B
;0B
;dB
;�B
<6B
<PB
<jB
<jB
<�B
<�B
<�B
=<B
=<B
=VB
=VB
=qB
=�B
=�B
>B
>BB
>�B
>]B
>�B
?.B
?HB
?HB
?�B
@OB
@�B
@iB
@�B
A�B
B'B
C-B
CGB
C�B
DB
DB
D3B
E�B
E�B
F%B
F?B
F�B
F�B
F�B
G+B
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
J	B
I�B
J#B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KxB
K�B
LB
LJB
L0B
LJB
L�B
L�B
M6B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
OBB
O�B
O�B
PHB
P�B
QB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
S@B
S[B
S[B
S@B
S[B
SuB
S�B
S�B
S�B
TFB
T�B
T�B
T�B
U�B
U�B
VB
V�B
V�B
V�B
WYB
WsB
WYB
WsB
W�B
W�B
W�B
XB
XEB
XEB
X_B
X�B
X�B
Y1B
YKB
YB
X�B
X�B
Y1B
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
[#B
[qB
[�B
\CB
\�B
\xB
\�B
\�B
\�B
\�B
]B
]/B
]IB
]dB
]~B
]�B
]�B
]�B
]�B
^B
^jB
^OB
^�B
^�B
^�B
^�B
_B
_!B
_pB
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`vB
`�B
`�B
aB
aB
aB
aB
aB
a-B
aHB
a�B
a�B
b4B
b�B
b�B
b�B
c B
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
d&B
d�B
d�B
d�B
eB
ezB
e�B
fB
f2B
f2B
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
gRB
gRB
g�B
g�B
g�B
h>B
hsB
h�B
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jKB
jB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kkB
kkB
k�B
k�B
lB
l"B
l=B
lWB
lqB
l�B
l�B
l�B
mB
m)B
mCB
mwB
mwB
m]B
m�B
m�B
m�B
m�B
nB
ncB
n}B
ncB
n�B
o B
oB
o B
o5B
oiB
p!B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qvB
qAB
q[B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
t�B
uB
u%B
u?B
utB
uZB
u�B
u�B
u�B
v`B
vzB
vzB
v�B
v�B
v�B
w2B
w2B
w2B
wLB
wfB
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�B
y	B
y	B
y>B
y$B
yrB
y�B
zB
zB
zxB
z�B
z�B
z�B
z�B
{B
{B
{JB
{�B
{�B
|B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}VB
}VB
}�B
}�B
~(B
~(B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105231  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191437  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191437  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191437                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041444  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041444  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                