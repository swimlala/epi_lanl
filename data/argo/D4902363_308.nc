CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-06T21:36:16Z creation;2018-12-06T21:36:22Z conversion to V3.1;2019-12-19T07:26:20Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20181206213616  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              4A   JA  I2_0576_308                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؖ2�Y  1   @ؖ3�9 @9-�(���d0xF�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @,(�@r�\@�{@�{A
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�A��BBBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B��{B�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C�
C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxl)Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�r�D��D��D�6D�yHD��HD��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�vDǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�6D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�9HD�vDնD��D�6D�vDֶD��D�6D�vD׶D��D�6D�vDضD��D�6D�vDٶD��D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�vDݶD��D�6D�vD޶D��D�6D�vD߶D��D�6D�vD�D��HD�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�?�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�{A�oA�{A�$�A�&�A�+A�-A�(�A�&�A�(�A�-A�/A�7LA�;dA�M�A�ZA�Q�A�O�A�S�A�hsA�hsA�`BA�hsA�ffA�hsA�jA�l�A�jA�l�A�n�A�l�A�jA�l�A�n�A�jA�jA�hsA�l�A�r�A�t�A�jA�jA�hsA�bNA�$�A¸RA��A���A�ZA��A�$�A�p�A�+A�~�A�l�A�t�A�%A�v�A��hA�33A��A��A�VA��7A���A�`BA��/A�`BA�A�bNA���A���A�=qA��A���A�XA�;dA�~�A�~�A��A��A��mA�
=A���A�ĜA�;dA���A�bNA��-A�jA��PA���A�r�A���A���A�|�A�7LA�(�A��-A�
A~�A|jAz��Ay%AxffAw�Aw�;Aw�Av�!Au�TAu+At9XAs|�As"�Ar�+Aq�Ap�9An�HAmhsAm�Al��Ak��Ak��Ak��Aj��Aj{AiVAh1Af5?Ad��Ac\)Aa��A_�A[?}AY�FAV�HAU�TAS�AQ��AP�HAPAN��AM�wAL��AL�DAK��AI"�AG�TAG+AF�/AF�jAF��AE�ADz�AB��AAK�AAVA@��A@ZA?�
A?�7A>�/A=��A<��A<=qA;A:�A8 �A7x�A6��A6$�A5�PA4�A3l�A2�A1S�A/�A/dZA/VA.�9A-��A,��A,9XA+��A*A(z�A'��A'K�A&��A&�9A&v�A&  A%XA$�HA$��A$ffA#p�A"��A!�mA|�AI�A\)A��AdZA�-A��A �Av�A1A�-A
=Az�AbNA^5AI�AE�A{A�AK�A��A&�A�TA �A
�A
�uA
~�A
^5A	�7A��A�A/A��A��A��A��AI�A��A��A�AK�A=qAp�A ��A M�@���@�S�@�G�@��9@�\)@�E�@��@��@�-@���@�/@�b@���@���@���@�r�@�I�@�(�@���@��
@�S�@�C�@�z�@�l�@���@��@�@�Q�@�1'@�K�@◍@�V@�$�@�@��@�@�/@��;@޸R@��@���@�dZ@�@���@��
@�A�@���@�j@��;@�t�@θR@�bN@�J@�C�@Ĵ9@���@���@��@�dZ@�=q@�O�@�/@�%@���@���@�  @��H@�E�@��T@�x�@�r�@�\)@��@���@��T@��@�33@�^5@���@��9@�z�@�b@�ƨ@��@��!@���@��@��F@��y@�V@���@�b@��+@�{@��T@���@���@�1@��H@�V@��T@���@�1@��m@��F@��H@���@�7L@�V@��@��j@���@�bN@��F@�+@��@��#@���@��@�r�@�bN@�Z@�I�@�1'@�b@�  @���@��w@�\)@�ȴ@�ff@��@�O�@��@�K�@�
=@��@���@�V@�$�@�O�@��F@�+@�
=@���@��y@�ȴ@��R@���@�n�@�J@�@��@��-@�hs@�?}@�%@���@��j@��j@��9@��9@��u@�1'@���@�+@��!@�E�@���@�%@���@���@�z�@���@��F@��
@��@���@���@��@�K�@�;d@���@��@���@�n�@�=q@�$�@���@��T@���@�hs@�?}@���@��/@�Ĝ@��@�1'@��w@�ƨ@�t�@�33@�+@�33@�+@��@���@��H@��y@��+@�n�@�ff@�E�@�-@��@�{@��@��h@�p�@�O�@��@�Ĝ@���@�A�@�w@l�@~ff@}`B@}O�@}?}@|��@|�/@|I�@{��@{�F@{t�@z��@z^5@z�@y�@y�#@y��@yhs@y&�@x�`@x�9@x�@xb@w|�@w
=@vȴ@vE�@u��@uO�@uV@tj@t9X@t�@s��@s��@s"�@r�@r�!@q��@q�^@qhs@p��@p��@p  @o��@oK�@n�@nff@n5?@m��@m�h@l�D@kS�@j�H@j�!@j��@j�\@jn�@j^5@j=q@j-@j�@i%@f�y@ep�@d�@dz�@cC�@b�\@a�^@a�7@ax�@a%@a%@`��@`��@`bN@`  @^�y@^5?@]��@]�@]/@\��@\�j@\�@\�D@[�m@Z�\@Y�@Y�7@XĜ@XQ�@Xb@W�@Wl�@W�@V�R@V$�@U��@U�h@U�@U�@U`B@T�@S��@S�@SC�@S"�@S@R��@RJ@Q�7@P�`@Pb@O�w@Ol�@N�+@L�j@K�m@KdZ@J�@J~�@J=q@I�@I�^@I��@I�7@Ihs@IG�@I�@H��@H�u@HA�@Hb@Hb@G�@G�;@G�@G�w@G�P@Gl�@GK�@G
=@F�@Fv�@E�T@E`B@DI�@C�m@C��@C�@CS�@C"�@B��@B�@A��@A�7@Ahs@AG�@A&�@@�`@@�u@@r�@@Q�@?�;@?|�@?\)@?;d@?+@?
=@>�y@>�@>�R@>��@>ff@>{@=�@=`B@=/@=V@<��@<�@<9X@<�@;��@;�F@;dZ@;33@:�H@:^5@:-@:-@:-@:-@:-@:-@9�@9�^@9hs@9�@8�`@8��@8A�@7�w@7�@7��@7l�@7|�@7l�@7\)@7;d@7+@6ȴ@6��@6ff@6V@6E�@5�@5��@5`B@4�@41@3�
@3��@3t�@3C�@3@2�\@2=q@1��@1X@0Ĝ@0�@0r�@0r�@0bN@0Q�@0A�@0 �@/��@/K�@/�@.��@.�R@.5?@.$�@-@-�-@-�h@-p�@-p�@-`B@-O�@-/@-V@,�@,��@,�D@,j@,I�@+�F@+t�@+t�@+t�@+S�@+"�@+@*��@*�\@*J@)��@)��@)�7@)X@(��@(�9@(��@(�@(bN@(A�@(b@'�;@'��@'�w@'��@'l�@';d@'+@'
=@&ȴ@&ff@&$�@%��@%�-@%p�@%?}@$��@$��@$�D@$(�@#�F@#33@"�\@"=q@"�@"�@"J@!��@!X@ ��@ �9@ b@�;@�;@�;@�;@��@�w@�w@|�@�@�y@ȴ@��@ff@5?@{@�@�@�T@@�@`B@�@�@��@I�@�F@@^5@J@�@�^@�7@G�@�`@Ĝ@�9@��@�u@�@�u@�u@r�@A�@A�@Q�@Q�@bN@bN@Q�@A�@ �@  @  @�@�;@�@+@��@��@�y@�@�@�y@��@�@E�@�@@�h@�@`B@�@��@�D@9X@9X@(�@�@�
@ƨ@��@��@�@t�@S�@33@33@33@33@33@33@"�@@�@��@��@�!@�!@�!@~�@M�@=q@-@-@-@�@��@�7@7L@&�@%@�`@��@�9@A�@�;@�@l�@l�@\)@�@��@��@��@��@��@�y@�R@v�@V@$�@��@�h@�@p�@p�@O�@/@V@�/@�@�D@j@Z@I�@9X@(�@1@ƨ@��@t�@S�@C�@"�@@
�H@
��@
��@
n�@
=q@
-@
�@	��@	�7@	hs@	7L@	7L@	%@�`@Ĝ@�u@r�@Q�@A�@ �@�@�;@�@��@|�@K�@K�@K�@+@
=@��@ȴ@v�@$�@��@p�@`B@O�@�@V@��@�@�j@j@Z@I�@9X@(�@1@ƨ@��@�@dZ@S�@C�@"�@��@��@��@�\@^5@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�{A�oA�{A�$�A�&�A�+A�-A�(�A�&�A�(�A�-A�/A�7LA�;dA�M�A�ZA�Q�A�O�A�S�A�hsA�hsA�`BA�hsA�ffA�hsA�jA�l�A�jA�l�A�n�A�l�A�jA�l�A�n�A�jA�jA�hsA�l�A�r�A�t�A�jA�jA�hsA�bNA�$�A¸RA��A���A�ZA��A�$�A�p�A�+A�~�A�l�A�t�A�%A�v�A��hA�33A��A��A�VA��7A���A�`BA��/A�`BA�A�bNA���A���A�=qA��A���A�XA�;dA�~�A�~�A��A��A��mA�
=A���A�ĜA�;dA���A�bNA��-A�jA��PA���A�r�A���A���A�|�A�7LA�(�A��-A�
A~�A|jAz��Ay%AxffAw�Aw�;Aw�Av�!Au�TAu+At9XAs|�As"�Ar�+Aq�Ap�9An�HAmhsAm�Al��Ak��Ak��Ak��Aj��Aj{AiVAh1Af5?Ad��Ac\)Aa��A_�A[?}AY�FAV�HAU�TAS�AQ��AP�HAPAN��AM�wAL��AL�DAK��AI"�AG�TAG+AF�/AF�jAF��AE�ADz�AB��AAK�AAVA@��A@ZA?�
A?�7A>�/A=��A<��A<=qA;A:�A8 �A7x�A6��A6$�A5�PA4�A3l�A2�A1S�A/�A/dZA/VA.�9A-��A,��A,9XA+��A*A(z�A'��A'K�A&��A&�9A&v�A&  A%XA$�HA$��A$ffA#p�A"��A!�mA|�AI�A\)A��AdZA�-A��A �Av�A1A�-A
=Az�AbNA^5AI�AE�A{A�AK�A��A&�A�TA �A
�A
�uA
~�A
^5A	�7A��A�A/A��A��A��A��AI�A��A��A�AK�A=qAp�A ��A M�@���@�S�@�G�@��9@�\)@�E�@��@��@�-@���@�/@�b@���@���@���@�r�@�I�@�(�@���@��
@�S�@�C�@�z�@�l�@���@��@�@�Q�@�1'@�K�@◍@�V@�$�@�@��@�@�/@��;@޸R@��@���@�dZ@�@���@��
@�A�@���@�j@��;@�t�@θR@�bN@�J@�C�@Ĵ9@���@���@��@�dZ@�=q@�O�@�/@�%@���@���@�  @��H@�E�@��T@�x�@�r�@�\)@��@���@��T@��@�33@�^5@���@��9@�z�@�b@�ƨ@��@��!@���@��@��F@��y@�V@���@�b@��+@�{@��T@���@���@�1@��H@�V@��T@���@�1@��m@��F@��H@���@�7L@�V@��@��j@���@�bN@��F@�+@��@��#@���@��@�r�@�bN@�Z@�I�@�1'@�b@�  @���@��w@�\)@�ȴ@�ff@��@�O�@��@�K�@�
=@��@���@�V@�$�@�O�@��F@�+@�
=@���@��y@�ȴ@��R@���@�n�@�J@�@��@��-@�hs@�?}@�%@���@��j@��j@��9@��9@��u@�1'@���@�+@��!@�E�@���@�%@���@���@�z�@���@��F@��
@��@���@���@��@�K�@�;d@���@��@���@�n�@�=q@�$�@���@��T@���@�hs@�?}@���@��/@�Ĝ@��@�1'@��w@�ƨ@�t�@�33@�+@�33@�+@��@���@��H@��y@��+@�n�@�ff@�E�@�-@��@�{@��@��h@�p�@�O�@��@�Ĝ@���@�A�@�w@l�@~ff@}`B@}O�@}?}@|��@|�/@|I�@{��@{�F@{t�@z��@z^5@z�@y�@y�#@y��@yhs@y&�@x�`@x�9@x�@xb@w|�@w
=@vȴ@vE�@u��@uO�@uV@tj@t9X@t�@s��@s��@s"�@r�@r�!@q��@q�^@qhs@p��@p��@p  @o��@oK�@n�@nff@n5?@m��@m�h@l�D@kS�@j�H@j�!@j��@j�\@jn�@j^5@j=q@j-@j�@i%@f�y@ep�@d�@dz�@cC�@b�\@a�^@a�7@ax�@a%@a%@`��@`��@`bN@`  @^�y@^5?@]��@]�@]/@\��@\�j@\�@\�D@[�m@Z�\@Y�@Y�7@XĜ@XQ�@Xb@W�@Wl�@W�@V�R@V$�@U��@U�h@U�@U�@U`B@T�@S��@S�@SC�@S"�@S@R��@RJ@Q�7@P�`@Pb@O�w@Ol�@N�+@L�j@K�m@KdZ@J�@J~�@J=q@I�@I�^@I��@I�7@Ihs@IG�@I�@H��@H�u@HA�@Hb@Hb@G�@G�;@G�@G�w@G�P@Gl�@GK�@G
=@F�@Fv�@E�T@E`B@DI�@C�m@C��@C�@CS�@C"�@B��@B�@A��@A�7@Ahs@AG�@A&�@@�`@@�u@@r�@@Q�@?�;@?|�@?\)@?;d@?+@?
=@>�y@>�@>�R@>��@>ff@>{@=�@=`B@=/@=V@<��@<�@<9X@<�@;��@;�F@;dZ@;33@:�H@:^5@:-@:-@:-@:-@:-@:-@9�@9�^@9hs@9�@8�`@8��@8A�@7�w@7�@7��@7l�@7|�@7l�@7\)@7;d@7+@6ȴ@6��@6ff@6V@6E�@5�@5��@5`B@4�@41@3�
@3��@3t�@3C�@3@2�\@2=q@1��@1X@0Ĝ@0�@0r�@0r�@0bN@0Q�@0A�@0 �@/��@/K�@/�@.��@.�R@.5?@.$�@-@-�-@-�h@-p�@-p�@-`B@-O�@-/@-V@,�@,��@,�D@,j@,I�@+�F@+t�@+t�@+t�@+S�@+"�@+@*��@*�\@*J@)��@)��@)�7@)X@(��@(�9@(��@(�@(bN@(A�@(b@'�;@'��@'�w@'��@'l�@';d@'+@'
=@&ȴ@&ff@&$�@%��@%�-@%p�@%?}@$��@$��@$�D@$(�@#�F@#33@"�\@"=q@"�@"�@"J@!��@!X@ ��@ �9@ b@�;@�;@�;@�;@��@�w@�w@|�@�@�y@ȴ@��@ff@5?@{@�@�@�T@@�@`B@�@�@��@I�@�F@@^5@J@�@�^@�7@G�@�`@Ĝ@�9@��@�u@�@�u@�u@r�@A�@A�@Q�@Q�@bN@bN@Q�@A�@ �@  @  @�@�;@�@+@��@��@�y@�@�@�y@��@�@E�@�@@�h@�@`B@�@��@�D@9X@9X@(�@�@�
@ƨ@��@��@�@t�@S�@33@33@33@33@33@33@"�@@�@��@��@�!@�!@�!@~�@M�@=q@-@-@-@�@��@�7@7L@&�@%@�`@��@�9@A�@�;@�@l�@l�@\)@�@��@��@��@��@��@�y@�R@v�@V@$�@��@�h@�@p�@p�@O�@/@V@�/@�@�D@j@Z@I�@9X@(�@1@ƨ@��@t�@S�@C�@"�@@
�H@
��@
��@
n�@
=q@
-@
�@	��@	�7@	hs@	7L@	7L@	%@�`@Ĝ@�u@r�@Q�@A�@ �@�@�;@�@��@|�@K�@K�@K�@+@
=@��@ȴ@v�@$�@��@p�@`B@O�@�@V@��@�@�j@j@Z@I�@9X@(�@1@ƨ@��@�@dZ@S�@C�@"�@��@��@��@�\@^5@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B%�B(�B)�B'�B(�B+B-B+B,B-B-B-B-B-B-B.B.B-B-B-B-B-B-B.B/B/B.B.B.B0!B,BoB�fBffB�ZB/BJ�B7LBYBQ�BQ�B6FB�B.B�BJB/B(�BuBB�B��BŢB�B�B�mB�mB�HB�B�^B�hB�bB�Bz�Bx�Bu�BdZBM�B5?B?}BT�BF�B2-BPB
�HB
��B
�3B
�+B
��B
s�B
�B
��B
�7B
m�B
dZB
]/B
B�B
D�B
8RB
9XB
B�B
E�B
G�B
@�B
1'B
(�B
&�B
!�B
�B
�B
�B
PB	��B	�B	�B	��B	��B	�B	��B	��B	�`B	�ZB	��B	��B	�9B	��B	��B	�=B	z�B	H�B	_;B	F�B	VB	C�B	9XB	C�B	?}B	7LB	6FB	0!B	/B	�B��B	VB	�B	�B	�B	�B	%B�B�B�TB	B��B��B�B�B�yB�B�B�BÖBȴB�BŢB��BB�^B�'B��B��B��B��B��B��B��B��B�\B�oB�\Bv�By�B�B�bB�bB�PB�JB�B� B~�B�Bz�BiyB_;BVB/B#�BI�BR�B<jB:^BJ�BJ�B=qBVBXBR�BR�B\)B^5B\)BYBR�BJ�BI�B@�B$�B(�B$�B2-BA�BH�BA�B5?B/B/B@�BC�BD�BD�B?}B6FB1'B)�B(�B%�B"�B&�B+B+B)�B.B �B(�B"�B"�B�B�B'�B,B&�B!�B �B�B6FB33B6FB6FB2-B,B�BB\B'�B/B)�B'�B33B49B.B/B6FB7LB6FB33B2-B,B�B �B"�B �B�B�B�B�BB�B �B,B'�B�BbB1BhBbB$�B/B0!B6FB8RB?}BI�BJ�BI�BG�BB�B@�BD�BH�BE�BC�BB�BM�BO�BJ�BE�BG�BT�BW
BZBcTBcTBe`Be`B^5B]/B^5BcTBffB^5Bn�Bp�BjB}�B�B~�Bz�By�Bz�B�B�=B�B�VB��B�uB�PB�DB��B��B��B��B��B��B��B��B��B��B��B�'B�FB�FB�FB�FB�?B�FB�FB�FB�3B�!B�!B�'B�-B�'B�!B�qBŢBǮBȴB��B�B��BȴB�#B�NB�sB�B�B�B�B�B�B��B��B��B	B	+B		7B	DB	VB	bB	\B	VB	PB	DB	JB	VB	bB	{B	�B	�B	$�B	,B	,B	'�B	/B	33B	33B	2-B	1'B	/B	0!B	2-B	2-B	5?B	6FB	8RB	:^B	=qB	=qB	?}B	?}B	B�B	D�B	E�B	H�B	J�B	L�B	J�B	R�B	^5B	]/B	^5B	dZB	ffB	gmB	gmB	iyB	k�B	n�B	n�B	w�B	z�B	z�B	|�B	}�B	}�B	}�B	}�B	�B	�B	�B	�B	�7B	�=B	�JB	�hB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�9B	�9B	�LB	�XB	�XB	�RB	�RB	�XB	�RB	�FB	�RB	�RB	�RB	�XB	�XB	�jB	�wB	�wB	��B	B	B	B	��B	ÖB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	ĜB	��B	�B	�B	��B	�B	�#B	�;B	�BB	�;B	�HB	�BB	�;B	�/B	�5B	�)B	�HB	�`B	�fB	�mB	�sB	�yB	�yB	�sB	�fB	�ZB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B	��B	��B
  B
  B
B
B
B	��B
1B
JB
PB
VB
bB
bB
oB
uB
uB
uB
uB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
"�B
"�B
%�B
%�B
&�B
%�B
&�B
%�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
)�B
+B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
,B
-B
0!B
1'B
0!B
1'B
1'B
0!B
0!B
0!B
/B
0!B
0!B
1'B
1'B
/B
0!B
0!B
/B
1'B
49B
5?B
5?B
5?B
49B
49B
49B
5?B
6FB
6FB
8RB
:^B
;dB
:^B
:^B
9XB
9XB
7LB
9XB
9XB
;dB
:^B
9XB
<jB
;dB
=qB
=qB
>wB
?}B
?}B
?}B
>wB
>wB
>wB
=qB
>wB
>wB
>wB
=qB
?}B
A�B
@�B
?}B
?}B
@�B
?}B
?}B
>wB
?}B
B�B
B�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
A�B
A�B
B�B
B�B
B�B
A�B
@�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
E�B
F�B
I�B
I�B
H�B
G�B
G�B
G�B
H�B
H�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
J�B
K�B
M�B
N�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
P�B
P�B
O�B
O�B
M�B
M�B
O�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
VB
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
YB
ZB
YB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
ZB
YB
\)B
]/B
]/B
]/B
]/B
]/B
]/B
[#B
XB
ZB
[#B
[#B
\)B
[#B
ZB
ZB
[#B
\)B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
aHB
aHB
aHB
`BB
aHB
aHB
aHB
aHB
bNB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
aHB
`BB
_;B
aHB
cTB
bNB
cTB
bNB
bNB
aHB
bNB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
e`B
e`B
e`B
e`B
e`B
dZB
hsB
hsB
hsB
hsB
hsB
gmB
gmB
hsB
gmB
iyB
iyB
jB
iyB
iyB
iyB
iyB
hsB
iyB
jB
jB
k�B
jB
k�B
k�B
k�B
k�B
jB
k�B
l�B
l�B
k�B
k�B
m�B
m�B
n�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
o�B
p�B
o�B
o�B
o�B
o�B
p�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
s�B
t�B
t�B
s�B
s�B
s�B
s�B
t�B
u�B
t�B
t�B
t�B
s�B
u�B
v�B
v�B
u�B
u�B
v�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B%�B(�B*B(
B)B+B-)B+B,"B-)B-)B-)B-)B-)B-)B./B./B-)B-)B-)B-)B-)B-)B./B/5B/5B.IB.IB.}B1'B.�B�B�Bv�B�B1�BL~B:�BZBSuBR�B8�B�B/�B �BvB/�B)�BB�B�BŢB��B�nB�B��B�B�hB�YB��B��B�uB�3B~�B{Bw�Bf�BQ B9�BA�BU�BH1B3�B B
�`B
�uB
��B
�0B
��B
w�B
�3B
��B
�rB
o�B
e�B
_B
EB
F�B
:xB
;JB
CGB
FYB
G�B
A B
2|B
*0B
'�B
# B
�B
 vB
�B
�B	��B	��B	�WB	�wB	��B	��B	�B	�FB	�B	�FB	֡B	�PB	��B	�B	��B	�B	}�B	M�B	abB	J=B	W�B	FYB	;�B	D�B	@�B	9$B	7�B	1AB	/�B	IB	B	�B	�B	5B	!B	?B	�B�B��B�FB	oB��B��B�B�aB�B��B�YB�?B�mB�	B��B�tBªB�{B�dB�|B��B�xB�$B��B��B��B��B�B� B�uB��By>B{�B�9B� B� B��B��B�B�B�B��B{�Bj�B`�BW�B2�B'�BK)BS�B>�B<�BK�BL0B?�BV�BX�BS�BS�B\]B^jB\xBYeBS�BK�BJrBA�B'�B*�B'8B3�BBABIBB'B6�B0�B0�B@�BDBEBEB@ B7B2GB+QB*KB'8B$ZB(>B+�B,"B+B.�B"B)�B#�B#�B�B�B(�B,�B'�B"�B!�BCB6`B3�B6�B6�B2�B,�B�BB4B(�B/�B*�B(�B3�B4�B/ B/�B6�B7�B6�B3�B2|B,�B �B!�B#�B!�BB�B�B�B�B?B!�B,�B(�B�B B
#B@BoB&2B0;B1AB7LB9XB@OBI�BK)BJ#BHBCGBA�BESBI7BFYBD�BC�BN<BP}BK�BF�BH�BU�BW�BZ�Bc�Bc�Be�Be�B_;B^OB_!Bd@BgRB_�Bo Bq[Bk�B~]B�oB}B{�Bz�B{�B��B��B�9B��B��B��B�<B�dB�B�B�B�B�:B�-B�dB�VB�hB��B��B��B�zB�zB�zB��B�tB��B�zB�zB��B��B��B��B��B��B�AB�B�B��B�B�<B�SBҽB��BۦB�B�B�B��B��B� B�B�9B�B�BB�VB	[B	zB		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	�B	
B	QB	%,B	,=B	,WB	(�B	/OB	3MB	3hB	2aB	1vB	/�B	0oB	2aB	2|B	5�B	6�B	8�B	:�B	=�B	=�B	?�B	?�B	B�B	EB	FB	H�B	KB	M6B	KxB	S[B	^jB	]�B	^�B	d�B	f�B	g�B	g�B	i�B	k�B	n�B	oB	xB	{B	{B	}<B	~(B	~(B	~]B	~]B	�GB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�2B	�B	�8B	�8B	�>B	�>B	�DB	�DB	�_B	�yB	�wB	�cB	��B	��B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	��B	�B	��B	�(B	�(B	�(B	�B	�"B	�0B	�EB	ňB	ΊB	�SB	�mB	յB	ٚB	ۦB	ߊB	�vB	ߤB	�bB	�vB	ߊB	ݘB	ބB	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�B	��B	�B	�+B	�B	�B	�2B	�2B	�DB	�(B	�(B	�(B	�"B	�^B	�0B	�.B
 OB
;B
;B
 iB	�]B	�cB
 iB
 �B
MB
�B
�B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
B
�B
B
 B
 B
�B
�B
!B
!B
 B
!B
#B
# B
#�B
$&B
$&B
$&B
$B
$B
# B
#:B
# B
&2B
&2B
'B
&2B
'B
&LB
(>B
($B
($B
'B
($B
($B
(XB
*KB
+B
,=B
,"B
,=B
,"B
,=B
,WB
,WB
,WB
-CB
-CB
,qB
-wB
0UB
1[B
0oB
1AB
1[B
0oB
0UB
0UB
/iB
0oB
0oB
1AB
1vB
/�B
0oB
0UB
/�B
1�B
4�B
5�B
5tB
5tB
4�B
4�B
4�B
5�B
6�B
6�B
8�B
:�B
;�B
:�B
:xB
9�B
9�B
7�B
9�B
9�B
;�B
:�B
9�B
<�B
;�B
=�B
=�B
>�B
?�B
?�B
?�B
>�B
>�B
>�B
=�B
>�B
>�B
>�B
=�B
?�B
A�B
@�B
?�B
?�B
@�B
?�B
?�B
>�B
?�B
B�B
B�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
A�B
A�B
B�B
B�B
B�B
A�B
@�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
C�B
EB
EB
E�B
F�B
I�B
I�B
IB
G�B
G�B
HB
IB
IB
LB
MB
L�B
L�B
MB
L�B
K�B
KB
K�B
NB
O(B
OB
P.B
P.B
Q4B
R B
R B
R B
R:B
QB
R:B
QB
Q4B
P.B
PB
N<B
NVB
PHB
S&B
TFB
TFB
TFB
TFB
T,B
U2B
VB
V9B
V9B
W?B
XEB
XEB
WYB
W?B
XEB
Y1B
Z7B
Y1B
Y1B
Y1B
YKB
YKB
ZQB
[=B
[WB
[qB
ZQB
YeB
\]B
]IB
]dB
]IB
]dB
]dB
]dB
[WB
X_B
ZQB
[WB
[WB
\]B
[WB
ZQB
ZkB
[WB
\xB
^jB
^OB
^�B
^jB
_pB
`vB
`vB
`\B
`vB
`vB
abB
a|B
bhB
bhB
abB
a|B
a|B
`vB
a|B
a|B
abB
a|B
bhB
abB
a|B
a|B
b�B
b�B
bhB
b�B
a�B
`�B
_�B
a�B
c�B
b�B
c�B
b�B
b�B
a�B
b�B
d�B
d�B
ezB
e�B
e�B
f�B
f�B
f�B
f�B
f�B
e�B
e�B
e�B
e�B
e�B
d�B
h�B
h�B
h�B
h�B
h�B
g�B
g�B
h�B
g�B
i�B
i�B
j�B
i�B
i�B
i�B
i�B
h�B
i�B
j�B
j�B
k�B
j�B
k�B
k�B
k�B
k�B
j�B
k�B
l�B
l�B
k�B
k�B
m�B
m�B
n�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
o�B
p�B
o�B
o�B
o�B
o�B
p�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
r�B
s�B
t�B
t�B
tB
s�B
tB
tB
t�B
u�B
t�B
t�B
uB
s�B
u�B
v�B
v�B
u�B
u�B
wB
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<I��<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.31(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812110033012018121100330120181211003301201812110200162018121102001620181211020016201812120021312018121200213120181212002131  JA  ARFMdecpA19c                                                                20181207063615  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181206213616  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181206213620  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181206213620  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181206213621  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181206213621  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181206213621  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181206213621  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181206213621  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181206213622                      G�O�G�O�G�O�                JA  ARUP                                                                        20181206215600                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181207153042  CV  JULD            G�O�G�O�Fı�                JM  ARCAJMQC2.0                                                                 20181210153301  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181210153301  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181210170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181211152131  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                