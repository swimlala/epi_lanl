CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-04-20T17:05:01Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ΰ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@        �@Argo profile    3.1 1.2 19500101000000  20180420170501  20230721230916  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�\��?�1   @�\��/s�@<B�\(���cȓt�j1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D�|�D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @\*@�{@�{A
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�A��BBBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dr�D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)r�D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxl)Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��GD��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD���D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�r�D��D��D�6D�vD��D��GD�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�vDǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�6D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�6D�vDնD��D�6D�vDֶD��D�6D�vD׶D��D�6D�vDضD��D�6D�vDٶD��D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�r�DݶD��D�6D�vD޶D��D�6D�vD߶D��D�6D�vD�D��D�6D�vD�D��D�6D�yGD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�?�D�iG111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�p�A�r�A�t�A�n�A�p�A�v�A�v�A�`BA�S�A�S�A�XA�ZA�O�A�/A�VA�ƨA��9A���A��A�r�A�dZA�VA�M�A�7LA�"�A���A���A���A��uA��DA��A��A�x�A�r�A�bNA�VA�Q�A�E�A�-A�bA���A���A���A�x�A�VA�33A�bA�x�A��A�t�A�=qA��A�M�A�ffA�  A��
A���A��A��7A�n�A�9XA�jA���A�S�A��\A��A���A�z�A��A�ƨA�+A�ZA��!A�G�A�$�A��A�bA��;A��DA�x�A�9XA��\A� �A��PA��mA�5?A�bNA�A�A��/A���A�9XA�$�A��RA�%A��^A���A�wAS�A~��A}ƨAz��AydZAx��Ax1'Aw�Av1'AtbAr�AqO�An�`Am��Am\)Am
=Al~�AlI�Ak�FAj�yAi�Ah�Af�+Adn�Ad  Ac�Aa�A`ĜA_|�A]�PA]"�A[�7AZ~�AY�AY��AY\)AX�`AW�FAV��AU;dAShsAP~�AOoAN�jAM&�AL�DAK��AKK�AJ�HAJ$�AIoAF�`AE�AEC�ADz�AC�-AB��ACt�AC�AC�#AB��AA�A@ZA?O�A=�mA<�`A<�+A;��A;oA:��A:�RA:ȴA:�A:E�A9\)A9A7�A7S�A69XA5�A4�A4~�A4bA3�TA3C�A2v�A1�A0��A0jA/�A/dZA.�A.~�A-K�A,��A,ZA+��A*$�A)�-A(�A(VA'��A'l�A&I�A%�hA%VA$n�A#+A!�A �\A�mA7LA��AJA?}A�yA~�A�-A�`AA�A\)Az�AI�AȴAO�A
=An�A�A�FAdZA�!AVA$�A��AA�A�AG�A�`A5?A�A�uA�TAS�A
�yA
�yA
�yA
^5A	��A	%AhsAJAXA%AM�A�#AS�AĜA-A�wA ��@��F@���@��;@�v�@�z�@��F@���@���@�33@��@�/@�dZ@�;d@�&�@�u@�F@��@�~�@�@���@���@�~�@�h@���@��`@��/@�@�9X@�x�@��@�Q�@�C�@�^5@�@ّh@ج@��@�v�@���@�`B@�1@�V@�O�@�  @���@��@˝�@�V@ɡ�@ȓu@Ǯ@�5?@ŉ7@ēu@��;@���@��T@�r�@��y@��@��!@�=q@�-@��D@��H@�ȴ@�~�@���@�7L@���@�z�@��
@�33@��@���@���@��/@��9@��/@�/@���@�+@��-@�%@�  @�33@��@�ƨ@�ƨ@�K�@��+@��h@�j@�33@�`B@��9@���@�bN@�1'@��@�\)@��@���@�%@��@�%@���@���@���@�r�@�|�@��@��y@�n�@�-@��@���@�z�@��;@�C�@�ff@��^@�x�@���@�hs@��j@�b@��m@�dZ@��@��y@���@�G�@��`@�Z@�^5@�=q@�V@�n�@�V@�-@�M�@�%@�bN@�ƨ@�o@�
=@��+@��#@�j@�t�@�o@���@���@�~�@�n�@��@�@�Ĝ@��;@�ƨ@��w@��@��P@�+@���@�^5@�5?@���@�/@�V@���@�z�@��@�K�@���@�~�@�n�@�V@�5?@���@�V@��u@��;@���@�\)@�33@�@�ff@���@���@��^@���@��@�r�@�@~��@~$�@}��@}�-@}�@}?}@|�@|�@|�@|�@|�@{o@y�^@yx�@yX@y%@x  @wl�@w�@vE�@v$�@u��@u��@uO�@t��@t�D@s�m@sdZ@sC�@r��@r�\@rJ@q��@q�@p��@p�9@p�@pb@o�P@n�y@n�R@nE�@m@m��@m�h@m�@mp�@m?}@m/@m/@m/@l��@l�/@l�/@l�@l�j@l��@lz�@l9X@k�@jn�@i��@i�#@i��@i��@i�#@i�#@i�#@i�#@i��@i��@i��@i��@iG�@i&�@i&�@h��@h��@h�9@hQ�@hb@g��@f�y@f��@f�+@fV@f{@e@e`B@e�@d�D@d1@c�F@ct�@b�!@a��@ax�@aX@a7L@a&�@a%@`�`@`��@`�@_�w@_�@^�y@^��@^E�@]�T@]p�@]O�@]/@\��@\��@\z�@\1@[C�@Zn�@Y��@Yx�@Y&�@X�`@X1'@X  @W�@W�;@W��@W�P@W�@V��@V@U?}@T�j@T��@T9X@S�
@S��@St�@St�@St�@St�@SC�@So@R�@R�H@R~�@RM�@R=q@R=q@R-@R�@Qx�@P��@P�u@PbN@O�w@OK�@O+@N��@Nȴ@N��@Nff@NV@N$�@M�T@M��@M`B@MO�@MO�@MO�@M?}@M/@MV@L�@L�j@L�D@L9X@L(�@L�@L1@K�
@Kt�@K33@K@J�!@J=q@I�#@Ix�@I7L@I&�@H��@H��@HQ�@H �@H  @G�@G�w@G|�@GK�@G;d@G+@G
=@F�y@F�R@F��@Fv�@Fff@FE�@F$�@F$�@E@EV@D�@D��@D�@Dz�@DZ@D(�@D1@C��@C�
@Cƨ@C��@CdZ@CS�@C33@B�@B��@B�!@B=q@BJ@A�@A��@A��@Ax�@A7L@@��@@�u@@Q�@?�@?�P@?\)@?K�@?;d@?;d@?;d@?;d@?�@>��@>v�@=�T@=@=�-@=��@=�h@=p�@=?}@<�@<��@<z�@;��@;�
@;t�@;"�@:��@:M�@:J@9�@9�7@9G�@9%@8��@8r�@8bN@8A�@8b@8  @7��@7l�@7K�@7;d@6��@6�y@6�@6��@6ff@6$�@6$�@5�T@5�@5/@5V@4��@4�/@4�j@4Z@3�m@3t�@3dZ@3C�@3@2�H@2��@2��@2��@2��@2-@1�#@1�7@17L@0�`@0Ĝ@0��@0A�@/�@.�R@.5?@-�T@-�-@-�-@-�-@-�-@-�-@-@-@-@-�-@-p�@-V@,�/@,�j@,��@,��@,j@,1@+"�@*�H@*�\@*-@)��@)��@)X@)G�@)7L@)7L@)&�@(�@'��@'K�@&�y@&ȴ@&ȴ@&�R@&��@&V@%�T@%�T@%�-@%p�@%`B@%?}@%�@%V@%V@$�@$�@$j@$�@#�F@#33@#o@"�!@"M�@"=q@"J@!�^@!hs@!7L@!&�@ �`@ �9@ �@ bN@   @��@��@�P@�P@�P@|�@�@�y@��@ff@{@�@�T@@��@�@O�@?}@�@�@�/@�j@�D@z�@j@Z@I�@ƨ@S�@��@��@~�@M�@J@��@��@��@hs@%@�@ �@ �@b@  @�@�;@��@��@�w@�@��@|�@\)@+@�y@�R@��@�+@ff@@@��@�@`B@/@�@�D@I�@9X@(�@�@�
@�F@��@�@dZ@33@�H@��@��@~�@~�@�\@�\@~�@�\@~�@n�@^5@-@�@�#@�#@��@x�@&�@Ĝ@�u@bN@bN@1'@b@�@|�@\)@+@
=@��@��@�y@�@�R@��@��@�+@V@$�@�@��@@�-@�-@�h@p�@?}@�@�@�/@�@j@I�@(�@�m@S�@o@
�@
�H@
��@
~�@
^5@
^5@
M�@
=q@
�@	�@	�^@	�7@	x�@	hs@	G�@	G�@	G�@	7L@	&�@	&�@	%@�`@��@Ĝ@�u@A�@ �@  @�w@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�p�A�p�A�r�A�t�A�n�A�p�A�v�A�v�A�`BA�S�A�S�A�XA�ZA�O�A�/A�VA�ƨA��9A���A��A�r�A�dZA�VA�M�A�7LA�"�A���A���A���A��uA��DA��A��A�x�A�r�A�bNA�VA�Q�A�E�A�-A�bA���A���A���A�x�A�VA�33A�bA�x�A��A�t�A�=qA��A�M�A�ffA�  A��
A���A��A��7A�n�A�9XA�jA���A�S�A��\A��A���A�z�A��A�ƨA�+A�ZA��!A�G�A�$�A��A�bA��;A��DA�x�A�9XA��\A� �A��PA��mA�5?A�bNA�A�A��/A���A�9XA�$�A��RA�%A��^A���A�wAS�A~��A}ƨAz��AydZAx��Ax1'Aw�Av1'AtbAr�AqO�An�`Am��Am\)Am
=Al~�AlI�Ak�FAj�yAi�Ah�Af�+Adn�Ad  Ac�Aa�A`ĜA_|�A]�PA]"�A[�7AZ~�AY�AY��AY\)AX�`AW�FAV��AU;dAShsAP~�AOoAN�jAM&�AL�DAK��AKK�AJ�HAJ$�AIoAF�`AE�AEC�ADz�AC�-AB��ACt�AC�AC�#AB��AA�A@ZA?O�A=�mA<�`A<�+A;��A;oA:��A:�RA:ȴA:�A:E�A9\)A9A7�A7S�A69XA5�A4�A4~�A4bA3�TA3C�A2v�A1�A0��A0jA/�A/dZA.�A.~�A-K�A,��A,ZA+��A*$�A)�-A(�A(VA'��A'l�A&I�A%�hA%VA$n�A#+A!�A �\A�mA7LA��AJA?}A�yA~�A�-A�`AA�A\)Az�AI�AȴAO�A
=An�A�A�FAdZA�!AVA$�A��AA�A�AG�A�`A5?A�A�uA�TAS�A
�yA
�yA
�yA
^5A	��A	%AhsAJAXA%AM�A�#AS�AĜA-A�wA ��@��F@���@��;@�v�@�z�@��F@���@���@�33@��@�/@�dZ@�;d@�&�@�u@�F@��@�~�@�@���@���@�~�@�h@���@��`@��/@�@�9X@�x�@��@�Q�@�C�@�^5@�@ّh@ج@��@�v�@���@�`B@�1@�V@�O�@�  @���@��@˝�@�V@ɡ�@ȓu@Ǯ@�5?@ŉ7@ēu@��;@���@��T@�r�@��y@��@��!@�=q@�-@��D@��H@�ȴ@�~�@���@�7L@���@�z�@��
@�33@��@���@���@��/@��9@��/@�/@���@�+@��-@�%@�  @�33@��@�ƨ@�ƨ@�K�@��+@��h@�j@�33@�`B@��9@���@�bN@�1'@��@�\)@��@���@�%@��@�%@���@���@���@�r�@�|�@��@��y@�n�@�-@��@���@�z�@��;@�C�@�ff@��^@�x�@���@�hs@��j@�b@��m@�dZ@��@��y@���@�G�@��`@�Z@�^5@�=q@�V@�n�@�V@�-@�M�@�%@�bN@�ƨ@�o@�
=@��+@��#@�j@�t�@�o@���@���@�~�@�n�@��@�@�Ĝ@��;@�ƨ@��w@��@��P@�+@���@�^5@�5?@���@�/@�V@���@�z�@��@�K�@���@�~�@�n�@�V@�5?@���@�V@��u@��;@���@�\)@�33@�@�ff@���@���@��^@���@��@�r�@�@~��@~$�@}��@}�-@}�@}?}@|�@|�@|�@|�@|�@{o@y�^@yx�@yX@y%@x  @wl�@w�@vE�@v$�@u��@u��@uO�@t��@t�D@s�m@sdZ@sC�@r��@r�\@rJ@q��@q�@p��@p�9@p�@pb@o�P@n�y@n�R@nE�@m@m��@m�h@m�@mp�@m?}@m/@m/@m/@l��@l�/@l�/@l�@l�j@l��@lz�@l9X@k�@jn�@i��@i�#@i��@i��@i�#@i�#@i�#@i�#@i��@i��@i��@i��@iG�@i&�@i&�@h��@h��@h�9@hQ�@hb@g��@f�y@f��@f�+@fV@f{@e@e`B@e�@d�D@d1@c�F@ct�@b�!@a��@ax�@aX@a7L@a&�@a%@`�`@`��@`�@_�w@_�@^�y@^��@^E�@]�T@]p�@]O�@]/@\��@\��@\z�@\1@[C�@Zn�@Y��@Yx�@Y&�@X�`@X1'@X  @W�@W�;@W��@W�P@W�@V��@V@U?}@T�j@T��@T9X@S�
@S��@St�@St�@St�@St�@SC�@So@R�@R�H@R~�@RM�@R=q@R=q@R-@R�@Qx�@P��@P�u@PbN@O�w@OK�@O+@N��@Nȴ@N��@Nff@NV@N$�@M�T@M��@M`B@MO�@MO�@MO�@M?}@M/@MV@L�@L�j@L�D@L9X@L(�@L�@L1@K�
@Kt�@K33@K@J�!@J=q@I�#@Ix�@I7L@I&�@H��@H��@HQ�@H �@H  @G�@G�w@G|�@GK�@G;d@G+@G
=@F�y@F�R@F��@Fv�@Fff@FE�@F$�@F$�@E@EV@D�@D��@D�@Dz�@DZ@D(�@D1@C��@C�
@Cƨ@C��@CdZ@CS�@C33@B�@B��@B�!@B=q@BJ@A�@A��@A��@Ax�@A7L@@��@@�u@@Q�@?�@?�P@?\)@?K�@?;d@?;d@?;d@?;d@?�@>��@>v�@=�T@=@=�-@=��@=�h@=p�@=?}@<�@<��@<z�@;��@;�
@;t�@;"�@:��@:M�@:J@9�@9�7@9G�@9%@8��@8r�@8bN@8A�@8b@8  @7��@7l�@7K�@7;d@6��@6�y@6�@6��@6ff@6$�@6$�@5�T@5�@5/@5V@4��@4�/@4�j@4Z@3�m@3t�@3dZ@3C�@3@2�H@2��@2��@2��@2��@2-@1�#@1�7@17L@0�`@0Ĝ@0��@0A�@/�@.�R@.5?@-�T@-�-@-�-@-�-@-�-@-�-@-@-@-@-�-@-p�@-V@,�/@,�j@,��@,��@,j@,1@+"�@*�H@*�\@*-@)��@)��@)X@)G�@)7L@)7L@)&�@(�@'��@'K�@&�y@&ȴ@&ȴ@&�R@&��@&V@%�T@%�T@%�-@%p�@%`B@%?}@%�@%V@%V@$�@$�@$j@$�@#�F@#33@#o@"�!@"M�@"=q@"J@!�^@!hs@!7L@!&�@ �`@ �9@ �@ bN@   @��@��@�P@�P@�P@|�@�@�y@��@ff@{@�@�T@@��@�@O�@?}@�@�@�/@�j@�D@z�@j@Z@I�@ƨ@S�@��@��@~�@M�@J@��@��@��@hs@%@�@ �@ �@b@  @�@�;@��@��@�w@�@��@|�@\)@+@�y@�R@��@�+@ff@@@��@�@`B@/@�@�D@I�@9X@(�@�@�
@�F@��@�@dZ@33@�H@��@��@~�@~�@�\@�\@~�@�\@~�@n�@^5@-@�@�#@�#@��@x�@&�@Ĝ@�u@bN@bN@1'@b@�@|�@\)@+@
=@��@��@�y@�@�R@��@��@�+@V@$�@�@��@@�-@�-@�h@p�@?}@�@�@�/@�@j@I�@(�@�m@S�@o@
�@
�H@
��@
~�@
^5@
^5@
M�@
=q@
�@	�@	�^@	�7@	x�@	hs@	G�@	G�@	G�@	7L@	&�@	&�@	%@�`@��@Ĝ@�u@A�@ �@  @�w@�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�XB�XB�XB�XB�XB�XB�^B�XB�XB�RB�RB�RB�RB�LB�FB�?B�9B�3B�3B�3B�-B�'B�!B�!B�!B�!B�B�B�B�B�B�B�B�B�B�!B�-B�3B�-B�3B�3B�3B�3B�-B�-B�-B�'B�'B�'B�B�B��B�B��B��B�Bo�BiyB`BBT�BR�BL�B;dB,BhB��B�fB��BĜB�wB�B�uB� Bp�BgmBdZBcTBbNB^5BXBVBP�BE�B=qB33B'�B�BB
�`B
�BB
�/B
�
B
ǮB
�9B
��B
��B
�{B
�+B
�B
}�B
t�B
bNB
YB
R�B
N�B
E�B
>wB
1'B
'�B
�B
PB
%B
%B
B
  B	��B	��B	�B	�B	�fB	�/B	��B	��B	ɺB	ÖB	�dB	�-B	��B	��B	��B	�bB	�PB	�DB	�1B	�B	{�B	v�B	l�B	[#B	?}B	49B	1'B	"�B	�B	 �B	�B	�B	�B	{B		7B	B��B��B�B�B��B		7B	VB		7B	B��B�B�ZB�`B�TB�BB�)B�#B�#B�5B�TB�TB�fB�fB�ZB�HB�)B�B�
B��B��B��B��B��B��BǮBƨBĜBÖB��B�wB�jB�^B�RB�FB�'B�B�B�B�B��B��B��B��B��B��B�uB�bB�\B�PB�DB�1B�B�B�B�B}�Bz�Bx�Bv�Bt�Bq�Bn�Bm�Bk�BjBiyBhsBffBe`BcTBaHB_;B_;B^5B\)BZBW
BT�BS�BR�BQ�BS�BS�BS�BS�BR�BP�BM�BK�BJ�BI�BH�BG�BF�BE�BD�BB�B@�B>wB<jB;dB:^B8RB6FB49B0!B-B+B(�B%�B!�B#�B#�B"�B"�B!�B"�B!�B�B�B �B �B �B�B%�B-B-B33B33B2-B49B5?B5?B5?B33B2-B2-B5?B7LB8RB8RB8RB<jB=qB>wB>wB=qB=qB=qB?}B@�BA�BA�BC�BD�BE�BF�BB�BB�BB�BA�BA�BC�BG�BI�BJ�BK�BK�BJ�BJ�BL�BP�BP�BVBXB[#B`BBe`BffBdZB`BBbNBbNBaHBffBiyBo�Br�Br�Bo�Bk�BiyBe`BgmBhsBiyBp�Bv�B{�B�B�+B�7B�7B�1B�B�B�=B�JB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�FB�?B�3B�?B�jB��BBÖBĜBŢB��B��B��B��B��B��B��B��B��B�B�B�B�#B�)B�;B�BB�ZB�fB�fB�fB�fB�mB�mB�yB�B�B�B�B�B�B��B��B	B	+B		7B		7B	
=B	DB	\B	uB	�B	�B	�B	�B	�B	�B	!�B	&�B	'�B	(�B	)�B	-B	/B	1'B	6FB	8RB	9XB	:^B	;dB	<jB	>wB	>wB	>wB	>wB	@�B	G�B	M�B	N�B	O�B	P�B	W
B	YB	[#B	_;B	`BB	aHB	aHB	bNB	bNB	cTB	e`B	hsB	iyB	l�B	o�B	s�B	v�B	y�B	y�B	z�B	z�B	|�B	}�B	�B	�B	�B	�1B	�1B	�7B	�7B	�=B	�=B	�=B	�=B	�DB	�JB	�PB	�PB	�PB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�3B	�9B	�?B	�?B	�LB	�RB	�^B	�^B	�dB	�wB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�)B	�5B	�;B	�BB	�HB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
1B
	7B

=B

=B

=B
DB
JB
PB
VB
VB
VB
\B
\B
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
-B
-B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
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
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
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
A�B
A�B
A�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
I�B
I�B
I�B
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
K�B
L�B
L�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
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
W
B
W
B
XB
W
B
W
B
XB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
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
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
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
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
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
r�B
r�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�uB�nB�qB��B�kB�_B�B��B��B�oB�YB�iB��B�B��B��B��B��B��B��B��B��B�lB��B��B��B�B�B�;B�MB�GB�6B�YB�OB�~B�tB�_B��B��B��B��B�?B��B�@B��B��B��B�(B�~B�B�;B��B�.B��B�rB�eBp�BlBc.BU�BTFBP�B?YB1�B B�OB��B��B��B�2B��B��B��BrxBhBd�Bc�BcNB_�BX�BWMBS�BG�B@,B6bB+0B#B�B
�BB
�B
�\B
�[B
κB
��B
��B
�gB
�B
�fB
��B
�B
|�B
e�B
Z�B
T�B
Q�B
HB
D[B
4]B
,{B
!�B
�B
�B
#B
�B
 �B	��B	�'B	�<B	�gB	�B	�}B	�LB	�IB	��B	ƺB	��B	�RB	�%B	��B	�PB	��B	�B	�(B	��B	�B	~XB	{B	q�B	b�B	CB	5NB	5B	${B	!B	!�B	! B	!�B	�B	�B	�B	�B��B�lB�tB�lB�1B		�B	�B	B	BB�TB��B�B�TB�B�aB��B�yB�!B�B�B�B�cB��B��B�B�BڦB�HB�&BӋBӗB��B�BB̋B��B��B� B��B��B��B�;B�5B�|B��B�uB�aB��B�$B��B��B��B�<B�wB�B��B�B�7B�DB�B��B�\B�%B�\B�[B�PB�B}uB{LBw�Bx�Bu�Bo�BokBl�Bk�Bj�BjzBg�BfBBf�Bd8B`^B_�B_B]�B\iBZ|BV�BVIBT�BSIBTBT4BU�BVRBU%BU�BRBNBK�BLBJ<BIaBH�BG}BFBE�BC�B@`B@�B=�B=UB9�B7|B6B3�B.�B+�B*�B*�B$CB$�B%B#�B#�B"�B#;B#B#B 
B!�B!B �B )B&�B0�B-�B44B4�B3�B4�B6B6�B6~B5wB3B3=B7eB9�B:B:RB: B?WB?�B@hB?�B?B>�B?�B@�BA�BB�BC%BD�BF�BG�BIQBE�BC?BB�BC�BC�BC�BH3BJ�BK�BL?BL�BK�BK�BN�BQ}BQ�BVOBXgB[B`Bf�BgfBf	Ba&Bc�BcXB`�BfpBi�Bp\Bs�BtBq+Bm.Bk�BfaBg�Bh�Bi�BqyBwUB{UB�B�
B�HB��B�0B�"B��B��B��B�B��B�JB�B��B�|B�gB��B��B�B��B�?B��B�SB��B��B�7B��B�nB��B��B�B��B�3B�
B��B�pB��B��B��BħBǍB��B��B��B�B��B�B�B�]BտB��B�[B�kB�kB��B��B��B��B�B�B�B��B�<B�gB�B��B�B�fB��B��B�B�/B��B	�B	�B		|B		�B	
�B	[B	^B	dB	�B	.B	B	B	.B	�B	"�B	'B	(2B	)OB	*�B	-�B	/�B	2B	6�B	8�B	9�B	:�B	;�B	<�B	>�B	>�B	>�B	>�B	A�B	H�B	N(B	OB	PAB	Q�B	W�B	YzB	[�B	_zB	`�B	a�B	a�B	b�B	b�B	c�B	e�B	h�B	jB	l�B	p/B	t4B	wRB	z6B	z!B	{8B	{jB	}�B	~�B	�[B	��B	��B	�xB	�hB	�mB	�pB	��B	�vB	�fB	�fB	��B	��B	�wB	�sB	��B	��B	��B	��B	�LB	��B	�<B	�B	� B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�B	�B	�0B	�&B	�)B	�]B	�MB	�|B	��B	�B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�MB	��B	��B	��B	��B	��B	��B	��B	�
B	�QB	�RB	�B	�#B	�FB	�OB	�_B	�4B	�6B	�@B	�FB	�eB	�yB	ׯB	��B	ܝB	޴B	ߜB	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�6B	�B	��B	�B	�B	�B	��B	��B	��B	��B	�B	� B	��B	��B	�1B	�B	�B	��B	��B	�B	�gB	�fB	�/B	�0B	�pB	�fB
 >B
 EB
QB
EB
[B
?B
[B
bB
lB
lB
PB
FB
HB
QB
OB
ZB
dB
oB
uB
�B
fB
bB
fB
vB
	�B

�B

�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
.B
�B
�B
�B
�B
�B
�B
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
'B
B
�B

B
�B
�B
 B
 B
 +B
!!B
">B
"=B
#B
$B
$B
$B
$B
$B
$B
$B
$cB
%rB
&&B
&B
&B
&B
'-B
';B
'NB
'1B
'OB
(yB
)=B
)lB
*dB
*�B
+mB
,hB
,NB
,}B
-nB
-lB
.�B
.aB
/QB
/^B
/kB
/RB
/nB
0�B
0bB
1_B
1�B
1[B
1[B
1vB
2�B
2�B
3[B
3�B
3�B
3�B
4xB
4qB
4{B
4}B
5�B
5�B
6�B
6~B
7�B
7�B
7�B
8�B
8}B
8�B
8�B
8�B
9�B
9�B
:�B
:�B
:�B
:�B
;�B
<B
=RB
=�B
>�B
?�B
?�B
?�B
?�B
?�B
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
A�B
A�B
B=B
C�B
C�B
C�B
D�B
D�B
FB
E�B
E�B
E�B
E�B
F4B
GaB
HB
IB
I�B
I�B
I�B
I�B
JB
J1B
J�B
K	B
KB
J�B
LB
LB
K�B
K�B
LB
LB
L"B
M1B
M>B
NYB
NB
NEB
OJB
PB
P,B
PFB
PDB
Q.B
QB
Q?B
Q2B
R:B
R1B
RaB
SAB
S?B
S(B
SB
SB
S,B
TpB
TLB
TeB
TIB
UiB
UBB
U6B
VHB
VMB
VHB
VTB
V<B
VHB
W[B
W@B
WNB
W]B
W@B
WBB
XFB
WIB
W�B
X�B
Y�B
Y^B
ZdB
ZtB
Z~B
Z�B
[jB
[\B
[B
[�B
[�B
\�B
]YB
]hB
]gB
]iB
]fB
]gB
]XB
^nB
]jB
^nB
^~B
^~B
^�B
^�B
^�B
_uB
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
dpB
d�B
d�B
duB
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
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
j�B
j�B
j�B
j�B
j�B
j�B
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
m�B
m�B
m�B
n2B
n�B
n�B
o�B
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
sB
r�B
r�B
sB
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2$<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.31 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310930162018103109301620181031093016  AO  ARCAADJP                                                                    20180420170501    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180420170501  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180420170501  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031093016  QC  PRES            @���D�s3G�O�                PM  ARSQCTM V1.1                                                                20181031093016  QC  PSAL            @���D�s3G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230916  IP                  G�O�G�O�G�O�                