CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:43:48Z creation;2022-06-04T17:43:48Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174348  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               uA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��Rq�1   @��R��[�@0G�z��ctj~��#1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A!��A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�ffB�ffB�  B�33B���B�33B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�B�B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  CL�C��C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�C3D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@l(�@�{@�G�A��A9p�AYp�A{
=A��A��A��A��AͅA݅A�A��BBBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB��{B�ǮB�ǮB�aHB��{B�.B��{B��{B�aHB�aHB�aHB�aHB�aHB��{B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB���B���B�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�qCJ>C!��C#��C%��C'�
C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK�>CM�>CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dr�D�Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D r�D �)D!l)D!�)D"l)D"��D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxl)Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�yGD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��GD�9GD�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�vDǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�6D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�6D�vDնD��D�6D�vDֶD��D�6D�vD׶D��D�6D�vDضD��D�6D�vDٶD��D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�vDݶD��D�6D�vD޶D��D�9GD�vD߶D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD��D��D�6D�r�D��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��)A��vA���A��AA��TA��ZA���A��A���A��A��TA��>A� 4A� �A�{A��A��A�YA��A�	A��A�	7A�A�
rA�
rA�DA��A�(A��A�A��]ẢlA˹$Aʢ�Aɍ�Aɪ�AɄ�A�S[Aė�A�J�Aç�A��/A�tTA���A�YA�E�A���A��A��'A�YA���A��~A�1A�͟A���A��'A�P}A��?A��+A���A��*A���A�j�A�kQA�b�A��A��wA��A���A�(�A��&A�"4A��IA��A�yrA�'�A���A��oA7�Av�rAq��Ak�7Ah�Ab�hA]��A[�zAZ_pAW��ATݘASe,ANRTAL�?AL��ALn�AIjAE�!AB��A?�sA=��A<9XA8�oA7�7A6�6A6�nA4�A2�KA1�A/	A./�A-�A-�1A-��A-�KA.�6A.�A-�
A-RTA,�	A,ĜA,��A,e�A+�A+{A)ĜA)�	A)g�A(p;A'=qA&�WA&�UA&�A$�EA#��A"�]A"�A"W?A!z�A!0�A �A OAO�A�A��A9XA�/A�KA
=A�YAU2A0�A�9Aq�A4AZA�PA�hAX�A�A��AIRA%A�wA�A�oA� Ao A=qACA��A��A�rA��A�hAb�A��A��A�eAc�A��A2�A�'A��A.IA�Aw2A]�AMjAMA
�>A
�$A
��A
`BA	��A	�A	��A	�DA	��A	h�A�A��A \A�HAQ�A��A6zA��Ak�Ay�A��AhsAeAV�A�+AFA�sA�&A�<AzA4nA�A�yA��A�YAn/AQ�A �A �f@���@��6@��8@���@�g8@��&@�hs@��@�*�@�!-@��@�ߤ@�� @�!-@�M@�F@�+@��@�@��@�@�z@�}V@��@��@���@��@��@��@�@@��f@�2�@��@�X�@��"@�b@�<6@���@� @�	@��B@�Ov@��@�1@��
@�~@��@�ں@୬@ߝ�@�͟@�0U@���@ݎ"@��@ܛ�@�U2@�($@�u@ۡ�@�F�@���@��'@ړu@�B[@� �@ٝ�@ؠ�@׫�@�$t@�s�@��@�j�@�C@�^5@�_@�!�@�>B@�=q@�_@��N@Ӆ�@���@� \@���@ά�@��@Ι1@�I�@�˒@�N<@�1�@��@˰�@���@�z@��@���@��K@�F�@ȋD@�I�@�>B@�/�@���@ǆ�@�Dg@�!�@ƽ<@�$�@��@ŭC@Ł@��@�w�@��@Ï�@�E9@��@�ȴ@¹$@�&�@�ݘ@��@���@��'@��_@�@�@���@��<@��b@���@���@�\�@��@���@��@�9X@��@�ԕ@�C�@��@�
�@���@�Vm@��@�j@�ԕ@�g�@�7L@�kQ@��o@���@�:�@���@�8�@���@��T@��C@�9�@��c@��z@�a|@�  @� \@���@��@���@��H@�x@�S�@���@��@�j@�&@��@��,@���@�xl@�@���@�=@���@�^5@�V@��@�[W@�Dg@�@��/@��D@�4n@��@��n@���@�]�@�33@��@��	@��v@���@��@�o�@�A�@��@���@�i�@�_@�B[@�%�@� �@� �@�@���@���@���@�A�@��@��@��}@��k@�?}@��M@��U@���@�Z@��@���@�t�@���@��5@��6@�R�@�
�@��@��@�7L@�)_@���@�C-@��@��^@��M@�b�@�#�@���@���@�r�@�A�@��>@��=@�+@��m@��+@�1'@�	@���@�dZ@�_p@�_p@�IR@�)_@��`@��@�V@��9@�e�@�Ɇ@��@���@���@�h�@�>B@��@��	@�U�@��@���@��o@��@�i�@�PH@�9X@�.�@��o@���@���@���@�Y�@�C@��@��@��$@�+k@���@�f�@�@O@��@��R@��.@�c�@�%�@���@�C�@���@���@��@��d@���@�IR@�@��p@��}@�=q@��r@���@��{@�\)@�6z@�'�@��@��z@�M�@�#:@���@�m]@�K�@��@�6@��H@��X@��n@���@���@�a�@�@@��p@���@�*�@�@�ƨ@��{@�@O@��@���@��c@��m@�}V@�U2@�:*@��d@���@�g�@���@���@�Ĝ@��}@�kQ@��@~��@~z@~^5@}�.@}�@|c�@|1'@{��@{��@{"�@z��@y�@y�@x�E@x��@x�o@x'R@x$@x1@w�;@wA�@w�@v�@v�,@v�@v��@v�1@v�x@v� @vq�@u��@u�@u�C@ue,@u�@t�D@t@sخ@s�@s��@s�*@s�V@s��@s{J@sH�@r�]@rC�@q�Z@q�z@q��@q�=@q��@p��@p��@p��@o�@@oa@o>�@oC@nB[@m��@l�/@l�@ke�@jq�@j�@i�H@i�~@i[W@i�@h��@h��@hZ@g�@g�q@f�@f1�@e�@e��@eϫ@e�@eo @d�e@c��@c�@b�R@a��@a��@a5�@`�4@`~(@`r�@`N�@`Q�@`I�@`>B@`'R@_�@_�q@_Y@^��@^�<@^8�@]�=@]%@\�5@\�?@\��@\�@\l"@\�Y@\?�@\z�@\��@\�@\A�@[~�@[�@Z�,@Z�m@Z��@Z�6@ZL0@Y��@Yo @Y^�@Y!�@X�U@X��@XH@X!@W�V@VW�@U�@U�-@UN<@U�@U�@T7@S1�@S+@S&@R�H@R?@RJ@Q��@Q\�@PɆ@P2�@Oy�@O>�@N��@N+k@N_@M��@M�@M��@M�9@M^�@L��@L��@L�o@Lj@Lb@K��@J�@J$�@I�@I��@I��@I�n@Ic@If�@IL�@I8�@I�@I�@H��@H��@H��@H�j@H�@HM@G��@Go�@F�'@FTa@F1�@F#:@E��@E��@E�@ET�@E5�@E�@D�|@D�)@D�@D�@D��@D|�@DQ�@D �@D~@C�@C�P@C.I@C�@B�"@B��@BGE@B.�@B	@B{@A�@Ahs@A�@@��@@�/@@��@?�W@?��@?��@>�@>L0@>.�@>e@=�o@=��@=!�@<��@<�.@<_@< �@;ƨ@;s@;+@:��@:��@:_�@9�T@9�@9��@9�@9q@8l"@7�r@7��@7X�@7o@6�'@6� @6^5@6Q@5��@5s�@5�@4��@4��@4I�@4"h@3�+@3ݘ@3��@2�m@2H�@1�9@1hs@0��@0N�@/�q@/�@/s@/Mj@/1�@/�@.��@.�b@-��@,�K@,�p@,Ĝ@,��@,<�@,	�@+��@+�{@+E9@+!-@*�@*� @*V@*+k@* �@)u�@)L�@)!�@(�@(��@(�o@(c�@(M@($@'�m@'��@'�:@'��@'qv@'Mj@'
=@&��@%�z@%��@%@$�)@$�Y@$�@#��@#|�@#{J@#+@"�"@"�s@"�@"��@"R�@!�@!�=@!&�@ ֡@ ��@ ��@ �$@ �@ e�@ "h@�0@�f@{J@o�@g�@]�@=@S@ں@�}@u%@@�@�#@��@�"@S&@�@�@1'@ƨ@��@t�@1�@�8@�@��@��@��@{�@��@�~@zx@j@IR@0�@�	@oi@(�@�g@�K@ƨ@�a@iD@��@�}@}V@{�@�+@��@�r@q�@E�@$�@�@�@�@��@��@�7@w2@G�@�@�@'R@'R@'R@"h@�@�@��@a@�@��@��@��@�r@\�@W�@8�@$�@�@�>@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��)A��vA���A��AA��TA��ZA���A��A���A��A��TA��>A� 4A� �A�{A��A��A�YA��A�	A��A�	7A�A�
rA�
rA�DA��A�(A��A�A��]ẢlA˹$Aʢ�Aɍ�Aɪ�AɄ�A�S[Aė�A�J�Aç�A��/A�tTA���A�YA�E�A���A��A��'A�YA���A��~A�1A�͟A���A��'A�P}A��?A��+A���A��*A���A�j�A�kQA�b�A��A��wA��A���A�(�A��&A�"4A��IA��A�yrA�'�A���A��oA7�Av�rAq��Ak�7Ah�Ab�hA]��A[�zAZ_pAW��ATݘASe,ANRTAL�?AL��ALn�AIjAE�!AB��A?�sA=��A<9XA8�oA7�7A6�6A6�nA4�A2�KA1�A/	A./�A-�A-�1A-��A-�KA.�6A.�A-�
A-RTA,�	A,ĜA,��A,e�A+�A+{A)ĜA)�	A)g�A(p;A'=qA&�WA&�UA&�A$�EA#��A"�]A"�A"W?A!z�A!0�A �A OAO�A�A��A9XA�/A�KA
=A�YAU2A0�A�9Aq�A4AZA�PA�hAX�A�A��AIRA%A�wA�A�oA� Ao A=qACA��A��A�rA��A�hAb�A��A��A�eAc�A��A2�A�'A��A.IA�Aw2A]�AMjAMA
�>A
�$A
��A
`BA	��A	�A	��A	�DA	��A	h�A�A��A \A�HAQ�A��A6zA��Ak�Ay�A��AhsAeAV�A�+AFA�sA�&A�<AzA4nA�A�yA��A�YAn/AQ�A �A �f@���@��6@��8@���@�g8@��&@�hs@��@�*�@�!-@��@�ߤ@�� @�!-@�M@�F@�+@��@�@��@�@�z@�}V@��@��@���@��@��@��@�@@��f@�2�@��@�X�@��"@�b@�<6@���@� @�	@��B@�Ov@��@�1@��
@�~@��@�ں@୬@ߝ�@�͟@�0U@���@ݎ"@��@ܛ�@�U2@�($@�u@ۡ�@�F�@���@��'@ړu@�B[@� �@ٝ�@ؠ�@׫�@�$t@�s�@��@�j�@�C@�^5@�_@�!�@�>B@�=q@�_@��N@Ӆ�@���@� \@���@ά�@��@Ι1@�I�@�˒@�N<@�1�@��@˰�@���@�z@��@���@��K@�F�@ȋD@�I�@�>B@�/�@���@ǆ�@�Dg@�!�@ƽ<@�$�@��@ŭC@Ł@��@�w�@��@Ï�@�E9@��@�ȴ@¹$@�&�@�ݘ@��@���@��'@��_@�@�@���@��<@��b@���@���@�\�@��@���@��@�9X@��@�ԕ@�C�@��@�
�@���@�Vm@��@�j@�ԕ@�g�@�7L@�kQ@��o@���@�:�@���@�8�@���@��T@��C@�9�@��c@��z@�a|@�  @� \@���@��@���@��H@�x@�S�@���@��@�j@�&@��@��,@���@�xl@�@���@�=@���@�^5@�V@��@�[W@�Dg@�@��/@��D@�4n@��@��n@���@�]�@�33@��@��	@��v@���@��@�o�@�A�@��@���@�i�@�_@�B[@�%�@� �@� �@�@���@���@���@�A�@��@��@��}@��k@�?}@��M@��U@���@�Z@��@���@�t�@���@��5@��6@�R�@�
�@��@��@�7L@�)_@���@�C-@��@��^@��M@�b�@�#�@���@���@�r�@�A�@��>@��=@�+@��m@��+@�1'@�	@���@�dZ@�_p@�_p@�IR@�)_@��`@��@�V@��9@�e�@�Ɇ@��@���@���@�h�@�>B@��@��	@�U�@��@���@��o@��@�i�@�PH@�9X@�.�@��o@���@���@���@�Y�@�C@��@��@��$@�+k@���@�f�@�@O@��@��R@��.@�c�@�%�@���@�C�@���@���@��@��d@���@�IR@�@��p@��}@�=q@��r@���@��{@�\)@�6z@�'�@��@��z@�M�@�#:@���@�m]@�K�@��@�6@��H@��X@��n@���@���@�a�@�@@��p@���@�*�@�@�ƨ@��{@�@O@��@���@��c@��m@�}V@�U2@�:*@��d@���@�g�@���@���@�Ĝ@��}@�kQ@��@~��@~z@~^5@}�.@}�@|c�@|1'@{��@{��@{"�@z��@y�@y�@x�E@x��@x�o@x'R@x$@x1@w�;@wA�@w�@v�@v�,@v�@v��@v�1@v�x@v� @vq�@u��@u�@u�C@ue,@u�@t�D@t@sخ@s�@s��@s�*@s�V@s��@s{J@sH�@r�]@rC�@q�Z@q�z@q��@q�=@q��@p��@p��@p��@o�@@oa@o>�@oC@nB[@m��@l�/@l�@ke�@jq�@j�@i�H@i�~@i[W@i�@h��@h��@hZ@g�@g�q@f�@f1�@e�@e��@eϫ@e�@eo @d�e@c��@c�@b�R@a��@a��@a5�@`�4@`~(@`r�@`N�@`Q�@`I�@`>B@`'R@_�@_�q@_Y@^��@^�<@^8�@]�=@]%@\�5@\�?@\��@\�@\l"@\�Y@\?�@\z�@\��@\�@\A�@[~�@[�@Z�,@Z�m@Z��@Z�6@ZL0@Y��@Yo @Y^�@Y!�@X�U@X��@XH@X!@W�V@VW�@U�@U�-@UN<@U�@U�@T7@S1�@S+@S&@R�H@R?@RJ@Q��@Q\�@PɆ@P2�@Oy�@O>�@N��@N+k@N_@M��@M�@M��@M�9@M^�@L��@L��@L�o@Lj@Lb@K��@J�@J$�@I�@I��@I��@I�n@Ic@If�@IL�@I8�@I�@I�@H��@H��@H��@H�j@H�@HM@G��@Go�@F�'@FTa@F1�@F#:@E��@E��@E�@ET�@E5�@E�@D�|@D�)@D�@D�@D��@D|�@DQ�@D �@D~@C�@C�P@C.I@C�@B�"@B��@BGE@B.�@B	@B{@A�@Ahs@A�@@��@@�/@@��@?�W@?��@?��@>�@>L0@>.�@>e@=�o@=��@=!�@<��@<�.@<_@< �@;ƨ@;s@;+@:��@:��@:_�@9�T@9�@9��@9�@9q@8l"@7�r@7��@7X�@7o@6�'@6� @6^5@6Q@5��@5s�@5�@4��@4��@4I�@4"h@3�+@3ݘ@3��@2�m@2H�@1�9@1hs@0��@0N�@/�q@/�@/s@/Mj@/1�@/�@.��@.�b@-��@,�K@,�p@,Ĝ@,��@,<�@,	�@+��@+�{@+E9@+!-@*�@*� @*V@*+k@* �@)u�@)L�@)!�@(�@(��@(�o@(c�@(M@($@'�m@'��@'�:@'��@'qv@'Mj@'
=@&��@%�z@%��@%@$�)@$�Y@$�@#��@#|�@#{J@#+@"�"@"�s@"�@"��@"R�@!�@!�=@!&�@ ֡@ ��@ ��@ �$@ �@ e�@ "h@�0@�f@{J@o�@g�@]�@=@S@ں@�}@u%@@�@�#@��@�"@S&@�@�@1'@ƨ@��@t�@1�@�8@�@��@��@��@{�@��@�~@zx@j@IR@0�@�	@oi@(�@�g@�K@ƨ@�a@iD@��@�}@}V@{�@�+@��@�r@q�@E�@$�@�@�@�@��@��@�7@w2@G�@�@�@'R@'R@'R@"h@�@�@��@a@�@��@��@��@�r@\�@W�@8�@$�@�@�>@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	OB	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	NVB	N�B	NVB	NVB	NVB	NVB	N�B	N�B	N�B	N�B	O(B	O\B	O\B	OB	OBB	O�B	OBB	N�B	N�B	O�B	N<B	L0B	T�B	c B	s�B	�(B
�BB
�OBzxB�*B$�BD3B`vB�B-B�+B��B�AB��BHBxRBzDBu?BgRBJ�B33B�B
�zB
��B
��B
� B
�B
�fB
��B
��B
�gB
zDB
o�B
aB
R B
J�B
EmB
>BB
9�B
2�B	��B	�yB	��B	�RB	��B	�]B	��B	�SB	}�B	��B	��B	}qB	z*B	q�B	gRB	fLB	d�B	dB	W�B	[�B	R�B	E�B	;�B	# B	�B	_B	GEB	=B	AUB	D�B	7�B	;�B	A�B	\�B	{�B	�JB	��B	��B	˒B	҉B	�YB	�7B	�xB	�vB	�B	��B	�tB	�TB	�B	�RB	�6B	��B	�6B	��B
B
�B
"B
vB
�B
�B

B
�B
�B
9B
�B
�B
�B
�B
2B
qB
OB
�B
;B
 �B
!bB
"�B
"B
"hB
�B
�B
 vB
 B
�B
5B
�B
�B
�B
B
�B
�B
QB
7B
B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
	�B
DB
B
�B
DB
	�B
	�B

	B
	�B

	B

=B
	�B

�B
gB
B
qB
�B
B
�B
�B
:B
�B
 B
�B
�B
�B

rB
xB
B
�B
dB
	�B
�B
�B
SB
tB
�B
�B
�B
�B
?B
tB
�B

	B

=B

	B

�B
	lB
KB
?B
SB
B
B
B
AB
'B
�B
�B
{B
 �B	�<B	��B	�B	�MB	�B	��B	�B	��B	�B	��B	�AB	�B	�B	�B	��B	�)B	�B	�)B	�)B	�B	��B	��B	�/B	�B	�B	�UB	�B	��B	��B	�/B	��B	�B	�CB	�B	�B	�B	�AB	�OB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�CB	�B	�wB	��B	�IB	�IB	�}B	�OB	�oB	��B	�B	�B	�B	��B	�B	�qB	�OB	�B	��B	�vB	�B	�B	��B	�B	��B	�RB	�sB	�B	�
B	�B	��B	�B	�B	�B	�tB	�tB	�B	��B	�B	�B	�B	�B	�B	��B	�_B	�_B	�B	��B	��B	�B	�B	��B	�B	�6B	�kB	�kB	�qB	�"B	�B	�=B	�B	��B	�B	�UB	�oB	�'B	�-B	�-B	�B	�B	�B	��B	��B	��B	�?B	�B	�FB	��B	��B	��B	��B	��B	��B	�^B	��B	�"B	��B	�BB	�BB	��B	��B	�B	�(B	�]B	��B	��B	�]B	�BB	�BB	��B	�}B	��B	�}B	��B
  B	�}B	�}B	��B	��B	�cB	�B	�HB	��B	�cB	�cB	�HB	�HB	�B	�B	��B	��B	��B	�HB	�cB	�.B	�}B	��B	�}B	��B	��B
  B
 �B
 �B
 B
;B
UB
�B
�B
�B
�B
�B
B
�B
{B
B
MB
B
3B
�B
B
B
SB
mB
�B
�B
B
�B
_B
KB
fB
	7B

�B
DB
xB
�B
dB
B
B
B
�B
pB
(B
�B
bB
bB
bB
�B
�B
�B
�B
 B
hB
�B
�B
:B
�B
�B
�B
�B
�B
,B
2B
�B
mB
�B
�B
+B
B
B
�B
+B
B
�B
1B
�B
�B
WB
CB
�B
�B
�B
�B
�B
�B
	B
kB
QB
kB
�B
~B
�B
B
B
B
�B
~B
�B
OB
�B
;B
pB
VB
�B
 vB
!B
!|B
!�B
!�B
"4B
"hB
"�B
"�B
#�B
#�B
#�B
$tB
%,B
%`B
%`B
%�B
&B
&LB
&2B
'B
&�B
'RB
'mB
'�B
'�B
'�B
'�B
($B
(sB
(�B
(�B
)B
)B
(�B
)�B
)yB
)�B
*B
*�B
+B
+6B
+6B
+6B
,B
,�B
,�B
,�B
-]B
.B
.IB
.cB
.cB
.�B
./B
.}B
/�B
0B
/�B
0B
0oB
0!B
0B
/�B
/iB
.�B
/B
/OB
/iB
/�B
/�B
0!B
0!B
0;B
0�B
0�B
1�B
2B
3MB
3�B
3�B
3�B
4nB
4�B
4�B
5%B
5�B
5�B
5�B
5�B
6`B
6zB
6`B
6zB
6zB
6�B
7B
6�B
6�B
6�B
7LB
8B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9�B
:B
:DB
:^B
:�B
:�B
:xB
:�B
:�B
:�B
:�B
:�B
:xB
:xB
:DB
:B
9�B
9�B
9�B
9XB
9$B
9	B
9>B
9rB
:DB
:�B
:�B
;B
;B
:�B
;JB
:�B
:�B
:�B
;B
<jB
<�B
<PB
;B
;0B
;B
:^B
:�B
;JB
<6B
<jB
<�B
<�B
="B
>B
>B
>(B
=�B
>�B
>�B
?B
?HB
?�B
?}B
?�B
@4B
@iB
AB
A;B
A�B
B�B
C�B
EB
F?B
F�B
GEB
F�B
F�B
F�B
F�B
F�B
GB
G�B
H�B
H�B
IB
IRB
IRB
I�B
I�B
IlB
I�B
H�B
J	B
JrB
J�B
K)B
KxB
L�B
L�B
L�B
L�B
M�B
NVB
N�B
N�B
OB
O�B
PbB
P�B
P�B
P�B
QNB
Q4B
Q4B
Q4B
Q4B
QB
QNB
Q�B
Q�B
Q�B
RTB
R�B
S@B
TFB
T{B
T�B
T�B
UB
UB
UgB
UMB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V9B
V�B
VmB
V�B
W$B
W?B
W?B
W?B
WYB
W�B
W�B
W�B
W�B
W�B
XB
XB
X+B
XEB
X+B
XEB
X_B
X_B
X_B
X�B
YB
X�B
X�B
YB
YB
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
Z7B
Z7B
ZQB
[#B
Z�B
[	B
[�B
\B
\)B
\)B
\)B
\xB
\�B
]B
]B
]IB
]dB
]�B
]�B
^B
^jB
^jB
^�B
_B
_B
_!B
_B
_pB
`BB
`�B
`�B
`�B
a-B
a|B
a|B
a�B
a|B
a�B
bhB
b�B
b�B
cB
c B
c:B
cTB
cTB
cnB
d&B
dtB
d�B
eB
e�B
fB
f�B
f�B
f�B
gB
gB
gB
gB
gRB
g�B
h�B
h�B
h�B
h�B
iB
iDB
iyB
i�B
i�B
i�B
jB
jeB
jB
j�B
j�B
kQB
k6B
kkB
k�B
k�B
k�B
lB
lB
l=B
l=B
lqB
lqB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
nIB
n}B
n�B
oB
oiB
o�B
o�B
o�B
o�B
o�B
p!B
p!B
pUB
p�B
p�B
q'B
q�B
q�B
q�B
q�B
q�B
rB
r-B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
s�B
tB
t9B
tTB
t�B
uB
utB
u�B
u�B
u�B
vB
vB
vFB
vFB
vFB
v+B
wB
wLB
w2B
wLB
wfB
wfB
w�B
xB
xRB
x�B
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
z*B
z*B
z^B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
|B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
}"B
}B
}qB
}qB
}�B
}�B
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	OB	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	N�B	NVB	N�B	NVB	NVB	NVB	NVB	N�B	N�B	N�B	N�B	O(B	O\B	O\B	OB	OBB	O�B	OBB	N�B	N�B	O�B	N<B	L0B	T�B	c B	s�B	�(B
�BB
�OBzxB�*B$�BD3B`vB�B-B�+B��B�AB��BHBxRBzDBu?BgRBJ�B33B�B
�zB
��B
��B
� B
�B
�fB
��B
��B
�gB
zDB
o�B
aB
R B
J�B
EmB
>BB
9�B
2�B	��B	�yB	��B	�RB	��B	�]B	��B	�SB	}�B	��B	��B	}qB	z*B	q�B	gRB	fLB	d�B	dB	W�B	[�B	R�B	E�B	;�B	# B	�B	_B	GEB	=B	AUB	D�B	7�B	;�B	A�B	\�B	{�B	�JB	��B	��B	˒B	҉B	�YB	�7B	�xB	�vB	�B	��B	�tB	�TB	�B	�RB	�6B	��B	�6B	��B
B
�B
"B
vB
�B
�B

B
�B
�B
9B
�B
�B
�B
�B
2B
qB
OB
�B
;B
 �B
!bB
"�B
"B
"hB
�B
�B
 vB
 B
�B
5B
�B
�B
�B
B
�B
�B
QB
7B
B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
	�B
DB
B
�B
DB
	�B
	�B

	B
	�B

	B

=B
	�B

�B
gB
B
qB
�B
B
�B
�B
:B
�B
 B
�B
�B
�B

rB
xB
B
�B
dB
	�B
�B
�B
SB
tB
�B
�B
�B
�B
?B
tB
�B

	B

=B

	B

�B
	lB
KB
?B
SB
B
B
B
AB
'B
�B
�B
{B
 �B	�<B	��B	�B	�MB	�B	��B	�B	��B	�B	��B	�AB	�B	�B	�B	��B	�)B	�B	�)B	�)B	�B	��B	��B	�/B	�B	�B	�UB	�B	��B	��B	�/B	��B	�B	�CB	�B	�B	�B	�AB	�OB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�CB	�B	�wB	��B	�IB	�IB	�}B	�OB	�oB	��B	�B	�B	�B	��B	�B	�qB	�OB	�B	��B	�vB	�B	�B	��B	�B	��B	�RB	�sB	�B	�
B	�B	��B	�B	�B	�B	�tB	�tB	�B	��B	�B	�B	�B	�B	�B	��B	�_B	�_B	�B	��B	��B	�B	�B	��B	�B	�6B	�kB	�kB	�qB	�"B	�B	�=B	�B	��B	�B	�UB	�oB	�'B	�-B	�-B	�B	�B	�B	��B	��B	��B	�?B	�B	�FB	��B	��B	��B	��B	��B	��B	�^B	��B	�"B	��B	�BB	�BB	��B	��B	�B	�(B	�]B	��B	��B	�]B	�BB	�BB	��B	�}B	��B	�}B	��B
  B	�}B	�}B	��B	��B	�cB	�B	�HB	��B	�cB	�cB	�HB	�HB	�B	�B	��B	��B	��B	�HB	�cB	�.B	�}B	��B	�}B	��B	��B
  B
 �B
 �B
 B
;B
UB
�B
�B
�B
�B
�B
B
�B
{B
B
MB
B
3B
�B
B
B
SB
mB
�B
�B
B
�B
_B
KB
fB
	7B

�B
DB
xB
�B
dB
B
B
B
�B
pB
(B
�B
bB
bB
bB
�B
�B
�B
�B
 B
hB
�B
�B
:B
�B
�B
�B
�B
�B
,B
2B
�B
mB
�B
�B
+B
B
B
�B
+B
B
�B
1B
�B
�B
WB
CB
�B
�B
�B
�B
�B
�B
	B
kB
QB
kB
�B
~B
�B
B
B
B
�B
~B
�B
OB
�B
;B
pB
VB
�B
 vB
!B
!|B
!�B
!�B
"4B
"hB
"�B
"�B
#�B
#�B
#�B
$tB
%,B
%`B
%`B
%�B
&B
&LB
&2B
'B
&�B
'RB
'mB
'�B
'�B
'�B
'�B
($B
(sB
(�B
(�B
)B
)B
(�B
)�B
)yB
)�B
*B
*�B
+B
+6B
+6B
+6B
,B
,�B
,�B
,�B
-]B
.B
.IB
.cB
.cB
.�B
./B
.}B
/�B
0B
/�B
0B
0oB
0!B
0B
/�B
/iB
.�B
/B
/OB
/iB
/�B
/�B
0!B
0!B
0;B
0�B
0�B
1�B
2B
3MB
3�B
3�B
3�B
4nB
4�B
4�B
5%B
5�B
5�B
5�B
5�B
6`B
6zB
6`B
6zB
6zB
6�B
7B
6�B
6�B
6�B
7LB
8B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9�B
:B
:DB
:^B
:�B
:�B
:xB
:�B
:�B
:�B
:�B
:�B
:xB
:xB
:DB
:B
9�B
9�B
9�B
9XB
9$B
9	B
9>B
9rB
:DB
:�B
:�B
;B
;B
:�B
;JB
:�B
:�B
:�B
;B
<jB
<�B
<PB
;B
;0B
;B
:^B
:�B
;JB
<6B
<jB
<�B
<�B
="B
>B
>B
>(B
=�B
>�B
>�B
?B
?HB
?�B
?}B
?�B
@4B
@iB
AB
A;B
A�B
B�B
C�B
EB
F?B
F�B
GEB
F�B
F�B
F�B
F�B
F�B
GB
G�B
H�B
H�B
IB
IRB
IRB
I�B
I�B
IlB
I�B
H�B
J	B
JrB
J�B
K)B
KxB
L�B
L�B
L�B
L�B
M�B
NVB
N�B
N�B
OB
O�B
PbB
P�B
P�B
P�B
QNB
Q4B
Q4B
Q4B
Q4B
QB
QNB
Q�B
Q�B
Q�B
RTB
R�B
S@B
TFB
T{B
T�B
T�B
UB
UB
UgB
UMB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V9B
V�B
VmB
V�B
W$B
W?B
W?B
W?B
WYB
W�B
W�B
W�B
W�B
W�B
XB
XB
X+B
XEB
X+B
XEB
X_B
X_B
X_B
X�B
YB
X�B
X�B
YB
YB
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
Z7B
Z7B
ZQB
[#B
Z�B
[	B
[�B
\B
\)B
\)B
\)B
\xB
\�B
]B
]B
]IB
]dB
]�B
]�B
^B
^jB
^jB
^�B
_B
_B
_!B
_B
_pB
`BB
`�B
`�B
`�B
a-B
a|B
a|B
a�B
a|B
a�B
bhB
b�B
b�B
cB
c B
c:B
cTB
cTB
cnB
d&B
dtB
d�B
eB
e�B
fB
f�B
f�B
f�B
gB
gB
gB
gB
gRB
g�B
h�B
h�B
h�B
h�B
iB
iDB
iyB
i�B
i�B
i�B
jB
jeB
jB
j�B
j�B
kQB
k6B
kkB
k�B
k�B
k�B
lB
lB
l=B
l=B
lqB
lqB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
nIB
n}B
n�B
oB
oiB
o�B
o�B
o�B
o�B
o�B
p!B
p!B
pUB
p�B
p�B
q'B
q�B
q�B
q�B
q�B
q�B
rB
r-B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
s�B
tB
t9B
tTB
t�B
uB
utB
u�B
u�B
u�B
vB
vB
vFB
vFB
vFB
v+B
wB
wLB
w2B
wLB
wfB
wfB
w�B
xB
xRB
x�B
x�B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
z*B
z*B
z^B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
|B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
}"B
}B
}qB
}qB
}�B
}�B
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104932  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174348  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174348  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174348                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024355  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024355  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                