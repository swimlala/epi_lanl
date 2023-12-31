CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-29T21:35:13Z creation;2017-11-29T21:35:16Z conversion to V3.1;2019-12-19T07:55:26Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171129213513  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_184                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�93A;� 1   @�93���@;�E8�4��d^Ʌ�oi1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @l(�@�{@�{A
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�RA��RBBBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{C��C��C��C��C	��C��C��C��C��C��C��C��C��C�
C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxl)Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD���D���D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��HD��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�2�D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�vDǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�6D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�6D�vDնD��D�6D�vDֶD��D�6D�vD׶D��D�6D�vDضD��D�6D�vDٶD��D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�vDݶD��D�6D�vD޶D��D�6D�vD߶D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�2�D�vD��D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�?�D�YH11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�ZA�^5A�^5A�`BA�^5A�\)A�`BA�?}A���A��A��A��`A��A���A���A�
=A��A��A��TA�~�A�;dA��A�1A���A��yA��mA��HA���A��hA�hsA�bNA�ffA�^5A�O�A�A�A��TA��!A��A�1'A�C�A�~�A��A�M�A��jA��-A��/A�|�A��A��FA��wA��HA��/A�A�=qA�(�A��A�A��mA���A���A���A���A��yA��HA��A�O�A��A��A���A�p�A�oA��A��yA��DA�5?A���A�?}A�G�A���A}��A}p�A}K�A|=qAzI�Ayp�Ax�Av�jAu�
Aup�AuoAt�jAt��As�7ArA�Aq�Ao|�An�jAm�Al��Al1Ak��Ak�Ak"�AjbNAj{Ah�Ah  Af�Ae��Ad�jAd�Ac��Acp�Ab�Aa�A_��A^�\A]�#A]��A]VAZ�yAX��AX�AW��AW��AW�AWhsAWK�AWoAU�hAS��AR�+AP�AN�9ANJAM�AM33AL��AK�-AI�AH �AGVAF^5AF1AE�TAE��AE7LAD�RADr�ADZADA�ADJAC�TAC�-AC��AC33AC�AB��AA�TA@��A?�7A>�HA>�uA>VA>1A=7LA;��A:JA9/A8�A7��A5�TA3�A2I�A1\)A0�yA0��A0�A0^5A0$�A/S�A.jA-��A+��A*��A*�A)�
A)hsA(E�A&9XA%|�A%S�A%�A$�`A$�`A$�`A$�HA$ȴA$�RA$��A$jA$bA#�A"�/A"5?A!��A!VA ��A VA�A�FA|�AG�A"�A��AE�AdZA1A�RA%AffA�TA�AE�A�AA��A�PAl�A��A(�A�;A?}A��A��Av�A�AO�A1'A��AS�A(�A
^5A	�FA	�A��AO�AbNA�A1'A�TAƨA�A�PAXA�A��A��A�mA �j@���@���@�@�?}@��D@���@��@�  @�;d@�R@��/@�ƨ@��^@�dZ@��@�@���@�(�@��@�33@�@�&�@�Ĝ@�9@�@�F@���@�dZ@ާ�@�/@���@�1'@��@պ^@���@��@�33@�z�@���@�9X@�33@ʸR@�~�@���@�`B@��@ȼj@��;@��@���@�|�@+@���@���@�Z@�E�@�9X@�K�@���@��@��
@�o@�x�@�Ĝ@���@�j@�1@��m@���@��@�M�@��@��^@���@��F@��@��@��^@��h@�p�@�?}@�&�@��`@���@�r�@�Q�@�(�@��;@���@��P@�dZ@�"�@���@�E�@�/@��@��@�&�@��@��@��w@�+@�ȴ@�n�@��@���@��@�(�@���@�
=@���@�(�@�ff@�%@�Z@�(�@� �@��@�ƨ@���@���@�t�@�
=@���@��@���@��@��u@�r�@�Z@�9X@��F@�C�@���@��y@��@�"�@��@��!@��+@�V@��@�x�@��@���@�Ĝ@�l�@���@���@���@�-@�M�@�E�@��@���@�hs@�/@��/@�z�@�9X@��@�S�@�ȴ@���@���@��@��j@��@�1'@���@�|�@��H@���@�n�@�J@��h@�G�@�/@���@�z�@�I�@� �@�  @�ƨ@��P@�dZ@�;d@�~�@��@�@�-@�$�@��-@��@��/@�z�@K�@~ff@}�@}��@}?}@|�D@|9X@|�@{��@{��@{C�@z^5@zJ@y�7@x��@w�@wl�@w\)@w;d@vv�@u`B@t�@t�@sS�@r�@r�H@r�@r��@rn�@r^5@q��@qG�@pĜ@p1'@p1'@pA�@p �@o��@n��@nE�@m�@m@mp�@mV@lZ@k��@kS�@j�H@j��@j�!@kC�@l1@l(�@k�m@kƨ@k�@kdZ@kC�@k33@k"�@j�@j~�@jn�@j�\@j=q@i��@i��@ix�@ihs@iX@iG�@i�@h��@gK�@fȴ@fff@f@e�-@e?}@e�h@ep�@e`B@e�T@d�@d�@c�F@c�F@cdZ@b�H@b-@a7L@`�`@`�u@`1'@_�@_�@_|�@_\)@_��@`b@`A�@`Ĝ@`Q�@_;d@^��@^v�@]��@]V@]@]`B@\j@\1@\1@\Z@\Z@\z�@\��@\�/@\�/@]�@]�@]/@]/@]/@\�j@\I�@\1@[�
@[ƨ@[��@[��@[S�@["�@Z�@Z��@Zn�@Y��@Y�#@Y��@Y�7@X��@XĜ@X�9@X��@X  @V�R@Up�@UV@T�@TZ@T(�@S�F@S@R^5@Q�^@Qhs@Q&�@P��@P�9@P��@P��@P�`@Q%@PĜ@P�u@Pb@O��@O|�@O+@O
=@Nȴ@NE�@M�-@MO�@MV@L��@L��@Lz�@LZ@K�F@K��@Kt�@K"�@I��@I�@Hr�@H  @Gl�@F��@F�@F�R@FE�@E��@EO�@E?}@E/@D�@C�F@C"�@C"�@B�!@B~�@BM�@B�@BJ@A�#@A��@A��@AX@A&�@@��@@bN@@ �@?�@?�w@?|�@>�y@>@=?}@<�/@<�j@<�@<��@<�D@<z�@<Z@<�@;�F@;�@;t�@;dZ@;S�@;33@;"�@;@:��@:�\@:�@9hs@9&�@8Ĝ@8�u@8�@8bN@8bN@8 �@7��@7;d@6�y@6�y@6�R@6@5�@5/@4�j@4j@4(�@3�m@3�
@3ƨ@3�F@3�@3dZ@3C�@3"�@2�H@2�\@2n�@2^5@2=q@1�@1�^@1��@1�7@1x�@1G�@1&�@0��@0�u@0Q�@0 �@/�;@/|�@/+@/�@.��@.�y@.ȴ@.��@.5?@.@-�@-�T@-��@,�@,�D@,�D@+�m@+t�@+dZ@+S�@+"�@+o@*�@*��@*��@*�\@)�#@)��@)��@(��@(bN@'\)@'+@'�@&��@&@%��@%�@%p�@%V@$�/@$�@$z�@$�@#�m@#33@"�H@"��@"~�@"n�@"n�@"M�@"^5@"^5@"M�@!��@!��@!��@!�7@!7L@!%@ ��@ �9@ �@ b@�;@��@|�@\)@\)@\)@�@
=@��@{@�@V@�@�/@��@j@9X@(�@�@��@�
@��@C�@o@��@��@~�@n�@=q@J@�^@��@�7@hs@G�@�@��@��@r�@1'@b@��@�w@�@|�@|�@\)@+@��@�@�@�@�@�y@�@ȴ@��@v�@$�@��@@�-@�@p�@p�@�/@�D@Z@�@1@��@ƨ@��@�@t�@S�@o@�H@��@�!@�!@n�@-@�@��@x�@7L@%@��@�9@Q�@�@�;@�;@�;@�;@�;@�w@�@��@�P@;d@��@ȴ@�R@��@v�@V@$�@@��@�-@p�@?}@/@�@V@�@��@�j@�@�@�@(�@�m@��@S�@"�@"�@o@@
�H@
��@
��@
��@
��@
�!@
�\@
n�@
M�@	��@	��@	�7@	x�@	hs@	G�@	&�@�`@�9@r�@A�@b@��@��@�w@��@�P@\)@�@��@�y@�@��@v�@ff@$�@��@�-@`B@?}@/@��@�j@z�@(�@1@ƨ@�@S�@"�@��@�!@��@~�@n�@^5@^5@M�@�@�@��@&�@%@ ��@ �9@ ��@ �@ r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�VA�ZA�^5A�^5A�`BA�^5A�\)A�`BA�?}A���A��A��A��`A��A���A���A�
=A��A��A��TA�~�A�;dA��A�1A���A��yA��mA��HA���A��hA�hsA�bNA�ffA�^5A�O�A�A�A��TA��!A��A�1'A�C�A�~�A��A�M�A��jA��-A��/A�|�A��A��FA��wA��HA��/A�A�=qA�(�A��A�A��mA���A���A���A���A��yA��HA��A�O�A��A��A���A�p�A�oA��A��yA��DA�5?A���A�?}A�G�A���A}��A}p�A}K�A|=qAzI�Ayp�Ax�Av�jAu�
Aup�AuoAt�jAt��As�7ArA�Aq�Ao|�An�jAm�Al��Al1Ak��Ak�Ak"�AjbNAj{Ah�Ah  Af�Ae��Ad�jAd�Ac��Acp�Ab�Aa�A_��A^�\A]�#A]��A]VAZ�yAX��AX�AW��AW��AW�AWhsAWK�AWoAU�hAS��AR�+AP�AN�9ANJAM�AM33AL��AK�-AI�AH �AGVAF^5AF1AE�TAE��AE7LAD�RADr�ADZADA�ADJAC�TAC�-AC��AC33AC�AB��AA�TA@��A?�7A>�HA>�uA>VA>1A=7LA;��A:JA9/A8�A7��A5�TA3�A2I�A1\)A0�yA0��A0�A0^5A0$�A/S�A.jA-��A+��A*��A*�A)�
A)hsA(E�A&9XA%|�A%S�A%�A$�`A$�`A$�`A$�HA$ȴA$�RA$��A$jA$bA#�A"�/A"5?A!��A!VA ��A VA�A�FA|�AG�A"�A��AE�AdZA1A�RA%AffA�TA�AE�A�AA��A�PAl�A��A(�A�;A?}A��A��Av�A�AO�A1'A��AS�A(�A
^5A	�FA	�A��AO�AbNA�A1'A�TAƨA�A�PAXA�A��A��A�mA �j@���@���@�@�?}@��D@���@��@�  @�;d@�R@��/@�ƨ@��^@�dZ@��@�@���@�(�@��@�33@�@�&�@�Ĝ@�9@�@�F@���@�dZ@ާ�@�/@���@�1'@��@պ^@���@��@�33@�z�@���@�9X@�33@ʸR@�~�@���@�`B@��@ȼj@��;@��@���@�|�@+@���@���@�Z@�E�@�9X@�K�@���@��@��
@�o@�x�@�Ĝ@���@�j@�1@��m@���@��@�M�@��@��^@���@��F@��@��@��^@��h@�p�@�?}@�&�@��`@���@�r�@�Q�@�(�@��;@���@��P@�dZ@�"�@���@�E�@�/@��@��@�&�@��@��@��w@�+@�ȴ@�n�@��@���@��@�(�@���@�
=@���@�(�@�ff@�%@�Z@�(�@� �@��@�ƨ@���@���@�t�@�
=@���@��@���@��@��u@�r�@�Z@�9X@��F@�C�@���@��y@��@�"�@��@��!@��+@�V@��@�x�@��@���@�Ĝ@�l�@���@���@���@�-@�M�@�E�@��@���@�hs@�/@��/@�z�@�9X@��@�S�@�ȴ@���@���@��@��j@��@�1'@���@�|�@��H@���@�n�@�J@��h@�G�@�/@���@�z�@�I�@� �@�  @�ƨ@��P@�dZ@�;d@�~�@��@�@�-@�$�@��-@��@��/@�z�@K�@~ff@}�@}��@}?}@|�D@|9X@|�@{��@{��@{C�@z^5@zJ@y�7@x��@w�@wl�@w\)@w;d@vv�@u`B@t�@t�@sS�@r�@r�H@r�@r��@rn�@r^5@q��@qG�@pĜ@p1'@p1'@pA�@p �@o��@n��@nE�@m�@m@mp�@mV@lZ@k��@kS�@j�H@j��@j�!@kC�@l1@l(�@k�m@kƨ@k�@kdZ@kC�@k33@k"�@j�@j~�@jn�@j�\@j=q@i��@i��@ix�@ihs@iX@iG�@i�@h��@gK�@fȴ@fff@f@e�-@e?}@e�h@ep�@e`B@e�T@d�@d�@c�F@c�F@cdZ@b�H@b-@a7L@`�`@`�u@`1'@_�@_�@_|�@_\)@_��@`b@`A�@`Ĝ@`Q�@_;d@^��@^v�@]��@]V@]@]`B@\j@\1@\1@\Z@\Z@\z�@\��@\�/@\�/@]�@]�@]/@]/@]/@\�j@\I�@\1@[�
@[ƨ@[��@[��@[S�@["�@Z�@Z��@Zn�@Y��@Y�#@Y��@Y�7@X��@XĜ@X�9@X��@X  @V�R@Up�@UV@T�@TZ@T(�@S�F@S@R^5@Q�^@Qhs@Q&�@P��@P�9@P��@P��@P�`@Q%@PĜ@P�u@Pb@O��@O|�@O+@O
=@Nȴ@NE�@M�-@MO�@MV@L��@L��@Lz�@LZ@K�F@K��@Kt�@K"�@I��@I�@Hr�@H  @Gl�@F��@F�@F�R@FE�@E��@EO�@E?}@E/@D�@C�F@C"�@C"�@B�!@B~�@BM�@B�@BJ@A�#@A��@A��@AX@A&�@@��@@bN@@ �@?�@?�w@?|�@>�y@>@=?}@<�/@<�j@<�@<��@<�D@<z�@<Z@<�@;�F@;�@;t�@;dZ@;S�@;33@;"�@;@:��@:�\@:�@9hs@9&�@8Ĝ@8�u@8�@8bN@8bN@8 �@7��@7;d@6�y@6�y@6�R@6@5�@5/@4�j@4j@4(�@3�m@3�
@3ƨ@3�F@3�@3dZ@3C�@3"�@2�H@2�\@2n�@2^5@2=q@1�@1�^@1��@1�7@1x�@1G�@1&�@0��@0�u@0Q�@0 �@/�;@/|�@/+@/�@.��@.�y@.ȴ@.��@.5?@.@-�@-�T@-��@,�@,�D@,�D@+�m@+t�@+dZ@+S�@+"�@+o@*�@*��@*��@*�\@)�#@)��@)��@(��@(bN@'\)@'+@'�@&��@&@%��@%�@%p�@%V@$�/@$�@$z�@$�@#�m@#33@"�H@"��@"~�@"n�@"n�@"M�@"^5@"^5@"M�@!��@!��@!��@!�7@!7L@!%@ ��@ �9@ �@ b@�;@��@|�@\)@\)@\)@�@
=@��@{@�@V@�@�/@��@j@9X@(�@�@��@�
@��@C�@o@��@��@~�@n�@=q@J@�^@��@�7@hs@G�@�@��@��@r�@1'@b@��@�w@�@|�@|�@\)@+@��@�@�@�@�@�y@�@ȴ@��@v�@$�@��@@�-@�@p�@p�@�/@�D@Z@�@1@��@ƨ@��@�@t�@S�@o@�H@��@�!@�!@n�@-@�@��@x�@7L@%@��@�9@Q�@�@�;@�;@�;@�;@�;@�w@�@��@�P@;d@��@ȴ@�R@��@v�@V@$�@@��@�-@p�@?}@/@�@V@�@��@�j@�@�@�@(�@�m@��@S�@"�@"�@o@@
�H@
��@
��@
��@
��@
�!@
�\@
n�@
M�@	��@	��@	�7@	x�@	hs@	G�@	&�@�`@�9@r�@A�@b@��@��@�w@��@�P@\)@�@��@�y@�@��@v�@ff@$�@��@�-@`B@?}@/@��@�j@z�@(�@1@ƨ@�@S�@"�@��@�!@��@~�@n�@^5@^5@M�@�@�@��@&�@%@ ��@ �9@ ��@ �@ r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BVBVBVBVBVBVBPBDBVB�B�B�B�BhBPBVB�B�B{BuB�B�B!�B!�B!�B"�B"�B!�B!�B#�B#�B&�B'�B&�B#�B�B�B�PB]/B"�B�#B�-B��B��B��B��B��B��B��B��B�=Bw�BcTBVBE�B8RB0!B"�B$�B%�B&�B$�B�B\BB
��B
��B
��B
��B
�B
�sB
�#B
��B
��B
ĜB
��B
�LB
��B
��B
�+B
�=B
�+B
~�B
n�B
l�B
gmB
\)B
S�B
T�B
R�B
N�B
K�B
C�B
7LB
33B
'�B
$�B
�B
�B
�B
{B
uB
hB
DB
1B
B	��B	��B	�B	�mB	�ZB	�NB	�5B	�B	��B	ĜB	�}B	�jB	�^B	�?B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�=B	|�B	y�B	o�B	gmB	ffB	dZB	aHB	]/B	VB	I�B	D�B	A�B	@�B	@�B	>wB	<jB	9XB	7LB	7LB	8RB	8RB	7LB	6FB	49B	49B	1'B	0!B	,B	'�B	!�B	�B	�B	�B	�B	{B	\B		7B	B��B��B��B�B�`B�;B�)B�#B�#B�#B�B�
B��B��BɺBÖB��B��B�jB�^B�9B�B�B�!B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�PB�%B�B}�B�B� B{�B|�B}�B}�B}�B}�B{�Bx�Bu�Bv�Bs�Bm�Bl�Bn�Bl�BjBffBffBe`B_;B[#BaHBbNB\)BXBZBYBXB\)B]/B]/B\)B[#B[#BYBW
BQ�BL�BG�BH�BM�BN�BL�BJ�BF�BF�BH�BG�BB�BA�B=qB;dB>wB>wB<jB;dB;dB:^B9XB9XB9XB:^B8RB33B-B2-B33B0!B+B/B33B33B5?B5?B49B,B)�B=qB<jB?}B@�B@�B@�BA�B?}B>wB=qB;dB>wB?}B=qB8RB?}B?}BB�BG�BH�BF�BE�BH�BG�BK�BO�BO�BP�BR�BT�BT�B[#B_;B_;B]/B^5BcTBgmBo�Bp�Bq�Br�Bs�Bs�Bu�Bv�Bx�Bx�By�B{�B}�B~�B~�B�B�B�=B�JB�bB��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�FB�jBÖBƨBƨBƨBŢBǮBɺB��B��B��B�B�B�#B�)B�/B�)B�;B�HB�HB�;B�;B�mB�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B��B��B	B	B	B	B	B	B	B	1B	DB	JB	PB	VB	VB	VB	oB	{B	�B	�B	�B	�B	�B	!�B	$�B	%�B	'�B	)�B	+B	+B	-B	.B	0!B	6FB	9XB	:^B	;dB	<jB	@�B	C�B	D�B	F�B	G�B	L�B	Q�B	Q�B	R�B	T�B	]/B	aHB	aHB	`BB	aHB	e`B	gmB	hsB	jB	jB	jB	jB	iyB	jB	iyB	jB	l�B	n�B	s�B	u�B	v�B	v�B	w�B	z�B	}�B	}�B	~�B	~�B	� B	�B	�1B	�=B	�PB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�3B	�FB	�LB	�RB	�XB	�RB	�FB	�XB	�^B	�XB	�^B	�jB	�wB	B	ÖB	ÖB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�BB	�HB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
	7B

=B

=B
	7B
1B

=B
JB
DB
JB
JB
PB
PB
PB
VB
VB
PB
VB
\B
VB
bB
bB
bB
bB
\B
\B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
!�B
"�B
"�B
#�B
$�B
$�B
%�B
&�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
.B
/B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
0!B
0!B
1'B
1'B
2-B
2-B
49B
49B
49B
49B
49B
49B
5?B
5?B
49B
6FB
6FB
49B
49B
5?B
6FB
7LB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
8RB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
D�B
D�B
D�B
E�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
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
L�B
L�B
M�B
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
N�B
O�B
O�B
O�B
O�B
O�B
O�B
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
O�B
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
S�B
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
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
^5B
]/B
^5B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
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
ffB
ffB
gmB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
jB
k�B
m�B
m�B
m�B
n�B
n�B
n�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BpBpBpBpBpBpB�B�B�B�B�B�BBoB(B�B�B�B�BaBCB B!�B!�B!�B"�B#B"4B"hB$@B$B'B(>B'mB%B�B�B��Bd�B)_B��B��B��B�'B�B��B�B��B��B�B�6B|�Bg�BY�BH�B;�B1�B%,B%zB&B'B%�B�B�B�B
��B
��B
��B
��B
�vB
��B
��B
�B
��B
żB
��B
��B
�sB
��B
��B
��B
��B
��B
p�B
m�B
h�B
^�B
UB
U�B
SuB
OvB
LdB
EB
9$B
4�B
)�B
&B
 BB
�B
?B
�B
�B
 B
JB
	B
�B	�PB	�FB	�B	��B	�,B	��B	��B	�B	өB	�B	�B	�VB	�B	��B	��B	�B	��B	�=B	��B	��B	��B	�B	�{B	�~B	cB	{�B	r-B	iyB	gmB	e,B	b4B	^OB	W�B	LdB	FYB	B�B	AoB	AB	>�B	="B	:*B	8B	7�B	8�B	8�B	7�B	6�B	4�B	4�B	1�B	0�B	-B	)*B	#:B	dB	�B	1B	?B	MB	�B	DB	GB	 4B�B�fB�UB�
B�HB�dB��BۦBیBٴB��B�FB�HB�^B��B��B�;B��B�dB�B�oB�B��B��B��B�iB�OB�IB�cB�]B�qB��B��B��B��B��B��B��B�VB�dB�CB�=B�#B�7B�B�gB�uB��B�1B�-B�4B�'B�B}VB}�B~wB~wB~]B~]B|jBy�Bv�Bw�Bt�BoiBnBo�Bm�Bk�Bg�Bg8Bf�BaHB]~BbhBc B]�BZB[�BZ�BYeB\�B]~B]~B\�B[�B[�BY�BXBSuBN�BJXBJrBN�BOvBM�BK�BHKBG�BI�BH�BC�BB�B?B=B?HB?HB=VB<B<B;JB:^B:*B9�B:�B8�B4TB/B3MB4B1�B-CB0UB49B4TB6B6FB5ZB.IB,"B=�B=VB@B@�BA BA BA�B@B?}B>�B<�B?�B@�B>�B:�B@�BA;BDBH�BIlBG�BF�BI�BH�BL~BP.BPHBQhBS@BU�BU�B[�B_�B_�B^B_!BdBh$Bo�BqBrBsBtBt9BvBwBy$By>Bz^B|PB~]BcB}B��B��B�B��B�}B�B�CB�NB�FB�fB��B��B��B� B�}B��B��B��B�B�:B�-B��B�sB�QB�=B�WB�qB�wB�]B�WB��B��B��B��B�"B��B��B�B�B�%B�1B�#B�B�B�B�mB�_B�qB�xBݘB��BߤB�B��B�\B��B�mB��B�B��B��B�LB�RB�JB�PB�VB�qB�wB�VB��B�qB	 �B��B��B	oB	�B	uB	�B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	)B	"hB	%FB	&B	(
B	*KB	+�B	+�B	-�B	.�B	0�B	6�B	9�B	:�B	;�B	<�B	@�B	C�B	D�B	F�B	HB	M6B	RTB	RTB	S[B	U�B	]~B	a�B	a�B	`�B	a�B	e�B	g�B	h�B	j�B	j�B	j�B	j�B	i�B	j�B	i�B	j�B	l�B	n�B	s�B	u�B	v�B	w2B	xRB	{JB	~BB	~(B	cB	HB	�iB	�mB	��B	��B	�jB	�pB	�NB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�2B	�$B	�QB	�CB	�cB	�iB	�iB	�OB	�iB	��B	��B	�|B	��B	�hB	��B	�`B	��B	��B	��G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�HB	�bB	�B	�HB	�HB	�NB	�B	�sB	ևB	�EB	�1B	�=B	�dB	�IB	�jB	�jB	�pB	�VB	�\B	�vB	�|B	�|B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�*B	�B	��B	��B	��B	��B	��B	� B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�6B	�6B	�VB	�BB	�.B
 OB
 OB
 OB
oB
AB
AB
aB
gB
gB
MB
{B
aB
MB
�B
�B
{B
gB
mB
�B
gB
tB
tB
�B
�B
	lB

rB

�B
	�B
�B

�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
 �B
 �B
 �B
 �B
!-B
!B
!B
!�B
!-B
!HB
"B
#B
# B
$&B
%,B
%,B
%�B
'B
'8B
&B
'8B
'B
'B
'8B
'B
($B
)B
(>B
($B
)*B
*0B
*B
*KB
*KB
*KB
+QB
+QB
+QB
,=B
-CB
-wB
.cB
/OB
/OB
/OB
/iB
/OB
0oB
1vB
1vB
1[B
0�B
0�B
1vB
1[B
2�B
2�B
4TB
4�B
4nB
4nB
4�B
4�B
5tB
5tB
4�B
6�B
6�B
4�B
4�B
5�B
6zB
7�B
6�B
6�B
7�B
7�B
7�B
7�B
7�B
8�B
8�B
9�B
9�B
8�B
;�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
D�B
D�B
D�B
E�B
EB
EB
D�B
E�B
F�B
F�B
F�B
FB
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
KB
J�B
K�B
K�B
LB
MB
MB
MB
MB
MB
N"B
MB
N"B
N"B
N"B
N�B
OB
OB
N�B
OB
OB
OB
O�B
O�B
O�B
O�B
PB
PB
P.B
PB
PB
PB
P.B
QB
Q4B
QB
QB
Q4B
PHB
R:B
S@B
S@B
TB
TFB
T,B
T,B
TFB
TFB
T,B
TFB
U2B
UB
UMB
U2B
UMB
TFB
U2B
UMB
V9B
V9B
V9B
V9B
V9B
V9B
W?B
Y1B
YKB
YKB
Y1B
Y1B
YKB
Y1B
Y1B
YeB
YKB
ZQB
ZQB
[WB
[qB
[WB
\xB
\]B
\]B
\xB
]dB
]~B
^jB
^OB
^jB
^OB
^jB
^jB
^jB
_VB
^jB
^�B
]�B
^jB
_�B
`�B
`�B
a|B
abB
abB
a�B
a|B
bhB
abB
abB
a|B
a�B
a�B
a|B
a�B
a�B
c�B
cnB
cnB
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
ezB
ezB
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
j�B
k�B
m�B
m�B
m�B
n�B
n�B
n�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%|�<#�
<#�
<9%~<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.31(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712050036362017120500363620171205003636201806221234122018062212341220180622123412201804050430182018040504301820180405043018  JA  ARFMdecpA19c                                                                20171130063512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171129213513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171129213515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171129213515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171129213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171129213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171129213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171129213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171129213516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171129213516                      G�O�G�O�G�O�                JA  ARUP                                                                        20171129215523                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171130153309  CV  JULD            G�O�G�O�F�ɚ                JM  ARSQJMQC2.0                                                                 20171201000000  CF  PSAL_ADJUSTED_QCD�@ D�  G�O�                JM  ARCAJMQC2.0                                                                 20171204153636  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171204153636  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193018  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033412  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                