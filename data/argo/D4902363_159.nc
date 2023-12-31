CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-15T21:35:06Z creation;2017-09-15T21:35:10Z conversion to V3.1;2019-12-19T08:01:36Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170915213506  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_159                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�&r�� 1   @�&s�l @:l����?�d͵s�h1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB/��B7��B?��BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @x��@�G�@�G�A
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�A��BBBBB'(�B.\)B6\)B>\)BFBNBVB^BfBnBw(�B~B�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxl)Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�2�D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�vDǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�6D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�2�D�vDնD��D�6D�vDֶD��D�6D�vD׶D��D�9HD�vDضD��D�6D�vDٶD���D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�vDݶD��D�6D�vD޶D��D�6D�vD߶D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��HD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�1'A�-A�+A�33A�;dA�9XA�;dA�=qA�;dA�33A�/A�"�A��A�{A�VA�%A���A��A���Aܧ�A�A�A�(�A��A�/AʓuA���A�r�AļjA�
=A���A��A�ĜA��^A��yA�/A���A�K�A�`BA�^5A�E�A���A�dZA�E�A�=qA��`A���A��+A��\A��A��^A�M�A�;dA�O�A�I�A��A��A�
=A��RA�S�A���A��PA���A��A���A�l�A���A��7A��A���A�^5A��\A�p�A�ƨA�XA�~�A���A��#A��A�O�A�K�A�G�A��A��#A�C�A�dZA�A��hA���A�ffA��A��A��A���A�ĜA���A��jA��FA��A�r�A���A���A���A�bA�ffA~��A}dZA{�;AzȴAy��Ax��Ax{Au��At�\At�Ar��Ao�mAn$�Al��Ak%Aj-Ai��Ai�^Ai��AiVAg�TAg�AfZAeAe��Ad{Ab�Ab-AadZA`��A_�mA^VA]��A\��A["�AYƨAXȴAW�
AW�AV�AU�^AUVATZAT{ASt�AR�uAQ�AP�!AN  AL=qAJ�`AJ1'AI�TAIt�AH��AHJAG/AFM�AE��AE��AEp�ADI�AC�AB�uAAp�A@�/A@~�A?��A?�7A>��A=��A<9XA;O�A:�yA:��A9��A97LA7p�A5��A5%A4r�A3��A2�yA2bA1\)A0��A0z�A0JA.��A-�A,�A+�TA+VA*��A*$�A)`BA)+A(��A($�A'�
A'7LA&�/A&=qA$�`A$�\A$9XA$�A#��A#�A#�
A#+A"�A!"�A ��A ��A�AC�A�;Az�AoA`BA��A�A��An�A��AC�A{A  A�
AdZA��AȴA�\A�A?}AVA�9AQ�A�wA;dAVA��A/An�A�A�A
��A
ZA
bA	ƨA	"�A5?AXA�A�DA�TA
=A�uAQ�A�A��A��A �A E�A J@���@��#@��-@��;@��@��h@�r�@�@�@�@�V@�M�@�@�@�^@�x�@�/@��@�@�@�{@�&�@܋D@�@�t�@�V@�I�@Ӿw@ҧ�@�J@�X@д9@��@���@�x�@��@̓u@�  @�J@��@�o@�v�@���@�%@�t�@�{@���@�K�@���@��@�=q@���@�O�@��u@���@�33@���@�M�@��`@���@��y@�n�@�J@��#@���@�p�@��@�bN@��w@�|�@�K�@�+@��@�v�@�M�@��@�@��@�(�@���@��@�;d@��H@�n�@�$�@�O�@�I�@��@�dZ@��y@�v�@��T@�G�@��j@�1'@�  @���@�l�@�C�@�v�@�{@���@���@�p�@�/@�Q�@��w@���@���@�V@���@�p�@��j@���@��R@�5?@�{@�@��-@��9@��m@�o@��#@�bN@�b@��;@���@��@�ȴ@��+@�ff@�$�@��@��h@��@���@� �@��@��P@��@�l�@�@���@�{@��T@���@���@���@��h@��@��@��@��@���@�t�@�C�@��y@��H@��@���@���@��!@�n�@��-@�`B@�G�@�%@�Ĝ@�bN@��@�ƨ@�33@�ȴ@�J@�/@��/@��j@���@��@��@��
@��P@�\)@��@��R@�^5@�5?@���@��T@��T@��#@��#@��#@��#@���@���@���@���@�x�@���@�I�@�@~ȴ@}�T@}V@|I�@|1@{�m@{�F@{dZ@z��@y�7@y�@x�@xb@w��@w;d@v�@v��@v$�@u�T@uO�@st�@r�@r�H@r��@r~�@r-@q��@q�@p�9@o�@o�P@ol�@o\)@n�R@nE�@n{@mO�@mV@l�D@l�@l1@k�
@k��@k��@kdZ@j��@j~�@i�@i�@i��@i%@hr�@h  @g�w@g�P@gl�@g+@f�R@f��@fV@e��@e?}@eV@d�/@d��@dj@d1@c��@c33@b��@b~�@bJ@aG�@a%@`��@`��@`�u@_�P@_\)@_;d@^��@^�+@^$�@]�@]�@\��@\j@[��@[33@[33@[dZ@[@Z��@Z�\@Yx�@XĜ@XbN@X �@W\)@W+@V��@Vȴ@Vv�@V{@U�-@U�h@Up�@U`B@U?}@U�@T�/@T�j@T�@Tj@T�@S@R~�@R�@Q��@QX@PĜ@PbN@P �@Pb@O�@O|�@OK�@Nȴ@Nv�@M�h@MV@L�@L�/@L��@L�j@Lj@L9X@K��@K�F@KS�@K"�@J��@J�\@J=q@I��@IG�@H��@H1'@G�;@Gl�@F�y@FE�@F$�@E��@Ep�@D�@D�@Dz�@DI�@C��@C�m@Cƨ@Ct�@B�!@BM�@B�@BJ@A�#@A��@A�7@Ax�@AX@A&�@A�@A�@A%@@��@@��@@��@@Ĝ@@Ĝ@@Ĝ@@��@@r�@@A�@?�@?|�@>��@=@=O�@=V@<��@<j@<I�@<I�@<I�@<I�@<(�@;�F@;@:�!@:M�@9��@9�^@9��@9hs@8��@8r�@8  @7�@6��@6�@6��@6�+@6�+@6ff@6{@5@5�@5�@4�/@4�D@3�F@333@2�@2��@2��@1�@17L@0�`@0��@0Q�@0 �@/�@/�;@/l�@.��@.��@.��@.$�@-�T@-�-@-p�@-O�@-?}@,��@,�D@,9X@,�@,1@+��@+�F@+��@+�@+�@+dZ@+S�@+C�@+"�@*�!@*M�@*-@*�@*J@)��@)��@)�@(Ĝ@(��@(1'@'�@'�w@'l�@';d@'
=@&��@&�y@&��@&ff@&$�@&{@&@%�-@%�h@%O�@%�@$�/@$��@$��@$j@$I�@#�
@#��@#�@#�@#t�@#t�@#dZ@#S�@#S�@#@"^5@"-@"�@!��@!��@!��@!x�@!x�@!X@!7L@!%@ �u@ r�@ bN@ bN@  �@�;@|�@�@�y@�@ȴ@ȴ@ȴ@��@ff@5?@{@@�@@p�@�@z�@j@I�@I�@(�@��@t�@dZ@@M�@�@��@��@7L@�u@�@r�@Q�@1'@ �@�@�;@��@��@�w@�@�@�@|�@K�@�@�y@�@�@ȴ@��@�+@�+@v�@v�@ff@E�@@��@�h@p�@�@�/@Z@I�@9X@9X@�@��@ƨ@C�@��@��@J@X@%@��@��@�u@�u@�@r�@bN@A�@1'@ �@b@  @�;@��@��@|�@;d@�@��@ff@5?@@�T@�h@`B@?}@�@�@�/@�j@�@��@��@�D@�D@z�@j@I�@(�@(�@�@�m@�
@�
@�
@ƨ@��@��@��@�@S�@"�@o@
�@
~�@
=q@	��@	�#@	��@	��@	��@	��@	x�@	G�@	%@��@��@r�@A�@1'@b@�w@��@l�@\)@�@�@�R@��@V@E�@@@�T@@�-@�h@p�@O�@?}@/@�@�@�@�@V@�@�/@��@�@z�@9X@1@�m@�F@�@"�@��@n�@^5@^5@^5@-@��@��@G�@7L@7L@&�@%@ �`@ 1'?���?�;d?��?��?��?���?��?��?��R?��R?���?�v�?�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1'A�1'A�-A�+A�33A�;dA�9XA�;dA�=qA�;dA�33A�/A�"�A��A�{A�VA�%A���A��A���Aܧ�A�A�A�(�A��A�/AʓuA���A�r�AļjA�
=A���A��A�ĜA��^A��yA�/A���A�K�A�`BA�^5A�E�A���A�dZA�E�A�=qA��`A���A��+A��\A��A��^A�M�A�;dA�O�A�I�A��A��A�
=A��RA�S�A���A��PA���A��A���A�l�A���A��7A��A���A�^5A��\A�p�A�ƨA�XA�~�A���A��#A��A�O�A�K�A�G�A��A��#A�C�A�dZA�A��hA���A�ffA��A��A��A���A�ĜA���A��jA��FA��A�r�A���A���A���A�bA�ffA~��A}dZA{�;AzȴAy��Ax��Ax{Au��At�\At�Ar��Ao�mAn$�Al��Ak%Aj-Ai��Ai�^Ai��AiVAg�TAg�AfZAeAe��Ad{Ab�Ab-AadZA`��A_�mA^VA]��A\��A["�AYƨAXȴAW�
AW�AV�AU�^AUVATZAT{ASt�AR�uAQ�AP�!AN  AL=qAJ�`AJ1'AI�TAIt�AH��AHJAG/AFM�AE��AE��AEp�ADI�AC�AB�uAAp�A@�/A@~�A?��A?�7A>��A=��A<9XA;O�A:�yA:��A9��A97LA7p�A5��A5%A4r�A3��A2�yA2bA1\)A0��A0z�A0JA.��A-�A,�A+�TA+VA*��A*$�A)`BA)+A(��A($�A'�
A'7LA&�/A&=qA$�`A$�\A$9XA$�A#��A#�A#�
A#+A"�A!"�A ��A ��A�AC�A�;Az�AoA`BA��A�A��An�A��AC�A{A  A�
AdZA��AȴA�\A�A?}AVA�9AQ�A�wA;dAVA��A/An�A�A�A
��A
ZA
bA	ƨA	"�A5?AXA�A�DA�TA
=A�uAQ�A�A��A��A �A E�A J@���@��#@��-@��;@��@��h@�r�@�@�@�@�V@�M�@�@�@�^@�x�@�/@��@�@�@�{@�&�@܋D@�@�t�@�V@�I�@Ӿw@ҧ�@�J@�X@д9@��@���@�x�@��@̓u@�  @�J@��@�o@�v�@���@�%@�t�@�{@���@�K�@���@��@�=q@���@�O�@��u@���@�33@���@�M�@��`@���@��y@�n�@�J@��#@���@�p�@��@�bN@��w@�|�@�K�@�+@��@�v�@�M�@��@�@��@�(�@���@��@�;d@��H@�n�@�$�@�O�@�I�@��@�dZ@��y@�v�@��T@�G�@��j@�1'@�  @���@�l�@�C�@�v�@�{@���@���@�p�@�/@�Q�@��w@���@���@�V@���@�p�@��j@���@��R@�5?@�{@�@��-@��9@��m@�o@��#@�bN@�b@��;@���@��@�ȴ@��+@�ff@�$�@��@��h@��@���@� �@��@��P@��@�l�@�@���@�{@��T@���@���@���@��h@��@��@��@��@���@�t�@�C�@��y@��H@��@���@���@��!@�n�@��-@�`B@�G�@�%@�Ĝ@�bN@��@�ƨ@�33@�ȴ@�J@�/@��/@��j@���@��@��@��
@��P@�\)@��@��R@�^5@�5?@���@��T@��T@��#@��#@��#@��#@���@���@���@���@�x�@���@�I�@�@~ȴ@}�T@}V@|I�@|1@{�m@{�F@{dZ@z��@y�7@y�@x�@xb@w��@w;d@v�@v��@v$�@u�T@uO�@st�@r�@r�H@r��@r~�@r-@q��@q�@p�9@o�@o�P@ol�@o\)@n�R@nE�@n{@mO�@mV@l�D@l�@l1@k�
@k��@k��@kdZ@j��@j~�@i�@i�@i��@i%@hr�@h  @g�w@g�P@gl�@g+@f�R@f��@fV@e��@e?}@eV@d�/@d��@dj@d1@c��@c33@b��@b~�@bJ@aG�@a%@`��@`��@`�u@_�P@_\)@_;d@^��@^�+@^$�@]�@]�@\��@\j@[��@[33@[33@[dZ@[@Z��@Z�\@Yx�@XĜ@XbN@X �@W\)@W+@V��@Vȴ@Vv�@V{@U�-@U�h@Up�@U`B@U?}@U�@T�/@T�j@T�@Tj@T�@S@R~�@R�@Q��@QX@PĜ@PbN@P �@Pb@O�@O|�@OK�@Nȴ@Nv�@M�h@MV@L�@L�/@L��@L�j@Lj@L9X@K��@K�F@KS�@K"�@J��@J�\@J=q@I��@IG�@H��@H1'@G�;@Gl�@F�y@FE�@F$�@E��@Ep�@D�@D�@Dz�@DI�@C��@C�m@Cƨ@Ct�@B�!@BM�@B�@BJ@A�#@A��@A�7@Ax�@AX@A&�@A�@A�@A%@@��@@��@@��@@Ĝ@@Ĝ@@Ĝ@@��@@r�@@A�@?�@?|�@>��@=@=O�@=V@<��@<j@<I�@<I�@<I�@<I�@<(�@;�F@;@:�!@:M�@9��@9�^@9��@9hs@8��@8r�@8  @7�@6��@6�@6��@6�+@6�+@6ff@6{@5@5�@5�@4�/@4�D@3�F@333@2�@2��@2��@1�@17L@0�`@0��@0Q�@0 �@/�@/�;@/l�@.��@.��@.��@.$�@-�T@-�-@-p�@-O�@-?}@,��@,�D@,9X@,�@,1@+��@+�F@+��@+�@+�@+dZ@+S�@+C�@+"�@*�!@*M�@*-@*�@*J@)��@)��@)�@(Ĝ@(��@(1'@'�@'�w@'l�@';d@'
=@&��@&�y@&��@&ff@&$�@&{@&@%�-@%�h@%O�@%�@$�/@$��@$��@$j@$I�@#�
@#��@#�@#�@#t�@#t�@#dZ@#S�@#S�@#@"^5@"-@"�@!��@!��@!��@!x�@!x�@!X@!7L@!%@ �u@ r�@ bN@ bN@  �@�;@|�@�@�y@�@ȴ@ȴ@ȴ@��@ff@5?@{@@�@@p�@�@z�@j@I�@I�@(�@��@t�@dZ@@M�@�@��@��@7L@�u@�@r�@Q�@1'@ �@�@�;@��@��@�w@�@�@�@|�@K�@�@�y@�@�@ȴ@��@�+@�+@v�@v�@ff@E�@@��@�h@p�@�@�/@Z@I�@9X@9X@�@��@ƨ@C�@��@��@J@X@%@��@��@�u@�u@�@r�@bN@A�@1'@ �@b@  @�;@��@��@|�@;d@�@��@ff@5?@@�T@�h@`B@?}@�@�@�/@�j@�@��@��@�D@�D@z�@j@I�@(�@(�@�@�m@�
@�
@�
@ƨ@��@��@��@�@S�@"�@o@
�@
~�@
=q@	��@	�#@	��@	��@	��@	��@	x�@	G�@	%@��@��@r�@A�@1'@b@�w@��@l�@\)@�@�@�R@��@V@E�@@@�T@@�-@�h@p�@O�@?}@/@�@�@�@�@V@�@�/@��@�@z�@9X@1@�m@�F@�@"�@��@n�@^5@^5@^5@-@��@��@G�@7L@7L@&�@%@ �`@ 1'?���?�;d?��?��?��?���?��?��?��R?��R?���?�v�?�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨB�/B��B��B��B��B�B�#B��B�9B�B��B��B��B��B�bBx�BgmBXBE�B33B9XB<jB8RB/B�BB��B�;B�BɺBÖBĜB��B�qB�^B�FB�B��B��B��B��B��B�7BaHBXBbNB_;BYBL�BF�BA�B8RB&�B�B�B�B!�B �B�BbB  B
��B
�B
�B
�NB
�B
��B
��B
��B
��B
��B
��B
��B
ɺB
ǮB
ÖB
�XB
�B
��B
�hB
�%B
y�B
m�B
bNB
ZB
S�B
N�B
G�B
:^B
0!B
-B
$�B
�B
+B
  B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�sB	�BB	�/B	�B	��B	��B	ƨB	B	�^B	�-B	�B	��B	��B	��B	�hB	�bB	�JB	�=B	�7B	�B	{�B	v�B	l�B	`BB	XB	VB	P�B	N�B	K�B	E�B	D�B	>wB	@�B	A�B	@�B	<jB	6FB	2-B	-B	"�B	�B	�B	�B	�B	bB	
=B	B��B	B	  B��B��B�B�fB�sB�fB�HB�;B�#B�B��B��B��BȴB��B��B�}B�jB�dB�^B�FB�FB�9B�-B�'B�B��B��B��B��B��B��B��B��B��B��B��B�oB��B�uB�PB�7B�Bz�Bu�Bq�Bu�Bx�Bv�Br�BhsBaHBcTBk�BiyBgmBe`Be`BcTB_;B_;B`BB]/B\)BW
BVBP�BQ�BP�BN�BJ�BO�BM�BM�BK�BI�BE�BA�B?}BD�B@�B>wB;dB=qB=qB<jB6FB-B33B7LB:^B9XB33B)�B49B7LB6FB6FB5?B2-B33B,B#�B+B/B5?B5?B33B2-B0!B/B,B(�B�B#�B(�B)�B49B7LB6FB9XB9XB9XB5?B:^B>wB>wB<jB;dB7LB;dB;dB?}B@�B>wB<jB=qB@�BA�BC�BA�BI�BO�BQ�BP�BQ�BS�BT�BT�BQ�BQ�BS�BYBZB[#B[#B\)B[#B[#B]/B`BBaHBbNBaHBaHBcTBcTBbNB`BBe`BhsBhsBiyBjBk�Bm�Bl�Bl�Br�Bu�Bv�By�Bz�B}�B� B�B�B�B�%B�+B�%B�=B�JB�PB�VB�VB�JB�hB�uB��B��B��B��B��B��B��B��B�B�B��B��B�B�B�'B�FBBÖBĜBĜBɺB��B��B��B��B��B��B��B��B�B�)B�/B�/B�)B�;B�BB�`B�fB�mB�sB�sB�sB�mB�fB�fB�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	+B		7B	PB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	(�B	(�B	(�B	(�B	(�B	(�B	(�B	'�B	'�B	%�B	&�B	,B	0!B	2-B	5?B	9XB	<jB	@�B	A�B	A�B	A�B	B�B	D�B	I�B	K�B	M�B	N�B	P�B	Q�B	S�B	S�B	T�B	VB	VB	^5B	`BB	`BB	`BB	aHB	bNB	dZB	gmB	hsB	l�B	n�B	n�B	m�B	p�B	r�B	r�B	u�B	u�B	w�B	y�B	z�B	z�B	{�B	{�B	{�B	}�B	~�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�1B	�1B	�=B	�=B	�DB	�PB	�\B	�\B	�bB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�9B	�LB	�RB	�RB	�RB	�XB	�^B	�jB	�qB	�qB	�qB	�qB	�qB	�wB	��B	��B	��B	B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�`B	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
1B
	7B
	7B
PB
PB
PB
VB
VB
VB
VB
\B
bB
bB
hB
hB
bB
oB
{B
�B
�B
{B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
#�B
$�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
+B
+B
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
/B
/B
0!B
0!B
0!B
0!B
1'B
0!B
2-B
2-B
33B
33B
33B
33B
33B
33B
2-B
2-B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
8RB
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
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
?}B
?}B
?}B
?}B
>wB
>wB
@�B
?}B
?}B
A�B
A�B
B�B
B�B
B�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
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
H�B
H�B
I�B
I�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
L�B
L�B
L�B
L�B
L�B
K�B
L�B
M�B
N�B
M�B
N�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
XB
W
B
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
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
]/B
\)B
\)B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
`BB
aHB
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
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
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
jB
jB
k�B
k�B
k�B
k�B
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
jB
k�B
l�B
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
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�KB�WB��BɺB��B�HB�B��B�B��B��B�iB�B�B�B�OB��B�CB�B��B�B}BjeB[�BI7B7fB;dB=<B9	B0�BB�B��B�B��B͟BżBňBªB��B�dB��B�B�KB�XB�,B�\B��B�Bi*B[#Bc:B`�B[=BO\BHfBCB:�B*B�B�B BB"B!bB/B@BaB
��B
��B
��B
�B
یB
өB
�B
�pB
�B
��B
��B
��B
�	B
�1B
ĜB
�B
�OB
�VB
�&B
�B
|B
o�B
d&B
[�B
UMB
P.B
IlB
=B
1�B
./B
&�B
�B
	�B
�B	�B	�B	�B	��B	�^B	��B	�2B	��B	��B	�|B	��B	�eB	��B	�B	�QB	��B	�HB	ȚB	��B	��B	�nB	��B	�FB	�B	��B	��B	�4B	�PB	�)B	��B	�%B	}<B	x8B	n�B	cnB	ZQB	W�B	Q�B	OvB	L�B	F�B	E�B	?�B	A�B	BB	A B	=VB	7�B	3hB	.}B	$@B	 �B	pB	~B	yB	�B	�B	�B	 4B	�B	 �B�B�B��B�sB�yB�B�B�\B�]B�B�BԯB��B�XBB��B��B��B�6B�JB�LB��B�?B��B��B��B��B�B�vB�tB�TB�:B� B�:B�\B��B�
B��B�
B�,B��B��B�B|�Bw�Bs�Bv�By>BwLBs�Bj�Bc�Bd�Bk�BjBh$BfBe�BdB`�B_�B`�B]�B\�BXBW
BR:BR�BQ�BP.BLBP�BN�BNpBLdBJrBF�BCB@�BE9BA�B?�B<�B>BB>B=<B7�B/�B4�B88B;B:*B5%B,�B5�B8RB7LB72B6FB3hB4TB.�B&�B,�B0;B5�B5�B3�B2�B1B0B-)B*B�B%�B*�B+�B4�B8B7LB9�B:*B:*B6�B;0B>�B>�B="B<PB8�B<�B<�B@BAUB?}B=�B>�BA�BB�BD�BCaBJ�BP�BR�BQ�BR�BT�BU�BU�BSBR�BT�BY�BZ�B[�B[�B\�B[�B[�B]�B`�Ba�Bb�Ba�Ba�Bc�Bc�Bb�BaHBe�Bh�Bh�Bi�BkBlBn/BmwBmwBs3BvFBwfBzxB{B~�B��B��B�mB��B��B��B��B��B��B��B��B��B�6B� B�,B��B�B�KB�B�]B��B��B�RB�6B�QB��B��B��B�B�GB�fB��B��B�B�SB�#B�)B�B�JB�6B�VB�vB�TBӏB�yB�xB�~BݘB��B߾B��B�B�B�B�B��B��B�B��B�8B�B��B�B�%B��B�B�B�B�8B�2B�fB�PB�HB�cB	 �B	oB	�B	�B	�B	�B	
	B	"B	�B	�B	�B	�B	+B	�B	B	B	;B	 'B	"4B	%,B	&2B	'8B	)*B	)*B	)*B	)DB	)*B	)DB	)DB	(>B	($B	&�B	'�B	,�B	0�B	2�B	5�B	9�B	<�B	@�B	A�B	A�B	A�B	B�B	E9B	J#B	LB	N<B	O(B	QNB	R:B	TaB	TFB	UgB	VmB	V�B	^�B	`�B	`�B	`�B	a�B	b�B	d�B	g�B	h�B	l�B	n�B	n�B	nB	qB	r�B	s3B	u�B	vB	x8B	zB	{0B	{0B	|B	|6B	|PB	~BB	HB	�UB	�UB	�oB	�{B	�mB	�tB	�_B	�_B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�:B	�&B	�:B	�@B	�LB	�fB	�>B	�B	�0B	�kB	�=B	�WB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�B	�#B	�0B	�B	�B	�(B	�.B	�4B	�TB	�FB	�MB	�FB	�SB	ևB	�eB	�]B	�xB	�]B	�xB	�xB	�~B	�~B	�jB	ޞB	�vB	�vB	�|B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	� B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�	B	�	B	�	B	�	B	�	B	�B	�B	�B	�B	�B	�$B	�B	�*B	�>B	�DB	�JB	�]B	�HB
 iB
UB
'B
'B
'B
[B
AB
oB
uB
gB
�B
�B
_B
_B
zB
zB
�B
	�B
	�B
�B
�B
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
 B
 �B
!B
!-B
!B
!�B
#B
#B
# B
#B
$B
$&B
$�B
%,B
%B
%B
%,B
$&B
%,B
'B
'B
'8B
'B
'8B
'RB
($B
)*B
)_B
*0B
+QB
+QB
,WB
,WB
-)B
-]B
-CB
-]B
.IB
.IB
.IB
.cB
/iB
/iB
/iB
0UB
0oB
0UB
0UB
1[B
0oB
2|B
2|B
3hB
3MB
3hB
3hB
3hB
3�B
2|B
2�B
4nB
5ZB
5�B
5�B
6�B
6zB
6zB
6zB
6�B
6�B
6�B
8�B
8lB
8�B
8�B
8�B
8�B
9�B
:�B
;B
;�B
;B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
?�B
?�B
?�B
?�B
>�B
>�B
@�B
?�B
?�B
A�B
A�B
B�B
B�B
B�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
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
IB
IB
H�B
H�B
I�B
I�B
IB
IB
I�B
I�B
J	B
I�B
KB
KB
L�B
MB
L�B
MB
MB
LB
M6B
N"B
OB
NVB
OBB
QB
Q4B
QB
RB
RB
R B
R B
S&B
S&B
SB
S&B
S&B
SB
S@B
S@B
S@B
S&B
S@B
S&B
T,B
TFB
UMB
UMB
UMB
U2B
V9B
W?B
W?B
WYB
X+B
W?B
X+B
XEB
Y1B
Y1B
YKB
YKB
Y1B
YeB
YeB
Z7B
ZQB
ZkB
[=B
[WB
[WB
[qB
\CB
\CB
\CB
\xB
\xB
\xB
]IB
\]B
\xB
]dB
^�B
^jB
^�B
_VB
_VB
_pB
_�B
^jB
_pB
_�B
`�B
`�B
`�B
`�B
a|B
`�B
a�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
d�B
d�B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
j�B
k�B
l�B
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
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D�e<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.31(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709250055242017092500552420170925005524201806221230492018062212304920180622123049201804050426022018040504260220180405042602  JA  ARFMdecpA19c                                                                20170916063505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170915213506  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170915213508  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170915213509  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170915213509  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170915213509  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170915213509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170915213509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170915213510  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170915213510                      G�O�G�O�G�O�                JA  ARUP                                                                        20170915215523                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170916153404  CV  JULD            G�O�G�O�F�3�                JM  ARCAJMQC2.0                                                                 20170924155524  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170924155524  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192602  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033049  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                