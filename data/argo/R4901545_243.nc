CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-12-31T13:00:20Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Hh   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ^�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  m�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ǰ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ֨   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191231130020  20191231130020  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2B  A   NAVIS_A                         0185                            052512                          863 @���gW1   @����}7�@7��v��cy&�x��1   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB ffB'��B0  B8ffB@  BG��BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\*@�{@�{A
=A;
=AYp�A{
=A��A��A��A��AͅA݅A�A��BBB(�B(�B&\)B.B7(�B>BF\)BNBVB^BfBnBw(�B~B�aHB�aHB�aHB�aHB��{B�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D��Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj��Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxl)Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��GD�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�vDǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�6D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�6D�vDնD��D�6D�vDֶD��D�6D�vD׶D��D�6D�vDضD��D�6D�vDٶD��D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�vDݶD��D�6D�vD޶D��D�6D�vD߶D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�yGD�G1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��/A��HA��`A��yA��A��A��A��A��A��A��A��A���A���A���A���A���A���A�A���A���A�A���A���A���A���A���A��A��/A�ƨAɺ^Aɩ�A�z�Aȧ�A�-AǋDA�|�AĲ-Aå�A���A�+A�1A�K�A���A���A���A�(�A���A�l�A��^A�v�A��A�?}A��HA��uA�dZA�/A�/A�
=A�7LA�+A��PA��PA��A�$�A�p�A�`BA�|�A�ZA�A�ZA�1A��DA���A���A�hsA��PA��A��#A�"�A�  A��mA���A�`BA�ZA�O�A���A��DA�Q�A��yA�|�A���A�l�A��A� �A��A�=qA�n�A�&�A���A��`A�r�A�33A��;A�A�bNA�VA�n�A~M�A|�jA|n�A{�#A{S�A{&�Ay�PAr��Ap�\AoAm��Al9XAk+Ai/Ag��Afz�Ad�AbjA`Q�A_?}A^�RA]S�A[C�AW�
AT�`AR��AQ�7AP5?AM�-AL �AKhsAJv�AI�TAI\)AHZAG�
AF�AD�ABbA?`BA<n�A;?}A:{A9oA8$�A6Q�A4��A3O�A1�^A0 �A.�A.�A,��A,E�A)�;A(��A'O�A&Q�A%
=A%�A$1A#�A"bA!��A z�A�7A��A=qA?}A��A�+A��AS�A�uAƨA�#Ax�A?}AM�AhsA��AoA�/A�uAE�A1A?}A�wA��A�
AVAJA
��A
�A
{A
{A	��A	XAĜA��A;dAjA�A
=A��A�wA?}A�RA=qA1'AJA�mA��A|�A @��!@�{@�/@��@��^@�v�@�@��-@��@��`@��@�@��y@蛦@�l�@�R@�{@��#@�/@�%@��
@�C�@�+@�E�@��@��#@�Ĝ@���@���@���@ؓu@�v�@��@��#@�/@�r�@�K�@��#@�9X@��@ˍP@�=q@��@ɲ-@�x�@��;@�\)@�v�@��T@���@ēu@�1'@��@�;d@�{@�b@��@���@��P@�dZ@��@�v�@�hs@�S�@�=q@���@�@��-@�%@�Q�@���@��!@�p�@�&�@��@�1@��H@�$�@���@���@�z�@�j@�bN@�A�@� �@���@�C�@��y@�V@��@�G�@��@��D@��@���@�5?@���@�I�@���@��@���@�O�@�p�@�x�@�V@�&�@�G�@��@���@��m@�33@��\@�n�@���@�$�@���@��^@�7L@��j@�b@�33@���@�hs@� �@�ƨ@�dZ@�C�@�+@�o@��R@�E�@��@�@��^@���@��7@�`B@�/@�V@��/@��u@�r�@�bN@�I�@��@��@�l�@�K�@�;d@�o@���@���@��@�J@�@�@��7@�x�@�G�@�V@���@��u@�Z@���@��@�33@�"�@�ff@�J@�@���@��7@�G�@��`@��@�(�@���@��w@��w@��F@���@�S�@�C�@�S�@�+@���@��+@�V@�E�@�E�@�=q@��#@��^@��@��@��@�`B@�&�@��@���@��9@�9X@�1@���@��w@��F@��@�|�@��@���@�E�@�-@�{@��T@��7@�G�@�%@�Ĝ@�r�@�A�@�b@���@�o@��y@���@�M�@�M�@�-@���@���@��h@�p�@�V@��j@�z�@�bN@�Z@��D@��9@�Z@���@�  @��@��m@��F@�l�@�^5@��@���@���@�p�@�?}@���@�r�@�@�w@l�@~ff@~{@~$�@}@}O�@}V@|�j@|I�@{ƨ@{�@{�@{t�@{S�@{o@z�\@z�@y�^@yhs@x�`@x�9@xA�@w�@w��@w|�@w+@v�y@v�@v�R@vff@vV@vE�@v5?@v@uO�@t��@tz�@t1@sƨ@s��@st�@s33@r��@r=q@q��@qG�@q&�@p�`@p��@p�u@pr�@pQ�@o�@n��@n��@nv�@n@m�-@mp�@m`B@mO�@m?}@m�@l��@l�/@l�/@l�/@l��@lj@lZ@l9X@k��@k"�@j��@jn�@i��@i%@h�`@h��@hb@g|�@g\)@g+@f�@f��@f$�@eV@d��@d�j@dz�@d�@cS�@b��@b��@bn�@b=q@a�^@aX@a7L@`��@_+@^�y@^�@^ȴ@^��@^5?@]��@]@]p�@]?}@\j@[��@[dZ@[33@["�@Z�@Z-@Y��@Y��@X�9@W�w@W\)@W�@V�y@V��@U��@U�@T��@T�@T�D@T9X@Sƨ@SdZ@R�@R~�@Q��@Q�#@Q��@QX@Q&�@PĜ@P�9@P�9@P�9@P��@P��@P�u@P�@Pr�@PbN@P1'@O�P@O+@Nff@Nff@N��@N�+@NE�@M�T@M`B@L�j@LZ@LI�@L(�@L�@K�m@K��@KdZ@KdZ@KS�@KS�@K33@J�@J^5@J�@JJ@I��@I��@Ix�@H��@Hr�@HbN@H �@G�;@G�w@G�P@F��@E`B@E�@D��@D(�@C�F@CdZ@C@B��@B��@B=q@A�@A��@Ahs@A7L@@��@@Ĝ@A%@@��@@��@@��@@�`@@��@@��@@ �@?|�@?
=@>��@>E�@>@=?}@<�/@<�j@<z�@<j@<9X@;ƨ@;��@;��@;C�@:��@:n�@:�@:�@:J@9�^@9X@9%@8��@8�9@8r�@81'@81'@8  @7�@7�@7�P@7l�@7l�@7K�@7+@7�@7�@6��@6ȴ@65?@5�@5�T@5�h@5�@4�@4��@4��@4z�@49X@3��@3��@3@2��@2��@2�\@2^5@2J@1��@1x�@1x�@1hs@0��@0�u@0Q�@/�@.��@.��@.��@.v�@.5?@.E�@.E�@.5?@-�@-�-@-`B@-/@-V@,�@,�/@,�j@,�j@,�@,�D@,j@,I�@,1@+�m@+�F@+t�@*��@*=q@*-@*-@*�@)��@)��@)��@)&�@)%@(�`@(Ĝ@(��@(�@'�;@'|�@'K�@'
=@&�R@&��@&�+@&V@&5?@%�@%��@%�h@%p�@%O�@%�@%V@$�/@$�D@$9X@$1@#��@#ƨ@#ƨ@#dZ@"�H@"��@"~�@"-@!�@!��@!7L@!&�@ ��@ Ĝ@ r�@ Q�@  �@   @�@|�@\)@�@
=@�@ȴ@V@$�@@�@�h@`B@?}@�@�@��@z�@�@�F@S�@o@�!@~�@J@�#@hs@&�@��@��@Q�@ �@  @�;@��@\)@;d@�@
=@��@�y@�@�R@��@ff@$�@�T@��@?}@�/@��@�D@z�@z�@(�@�
@�
@�F@��@t�@S�@33@��@n�@^5@M�@=q@-@�@�#@hs@�@bN@b@�@�;@�@�P@|�@l�@K�@��@��@v�@$�@@�-@�@p�@`B@/@�/@��@�j@�j@�@�@�@�@��@��@�D@Z@9X@1@�m@�
@ƨ@��@�@dZ@@
�\@
n�@
M�@
�@	��@	��@	�#@	�^@	X@��@�`@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��/A��HA��`A��yA��A��A��A��A��A��A��A��A���A���A���A���A���A���A�A���A���A�A���A���A���A���A���A��A��/A�ƨAɺ^Aɩ�A�z�Aȧ�A�-AǋDA�|�AĲ-Aå�A���A�+A�1A�K�A���A���A���A�(�A���A�l�A��^A�v�A��A�?}A��HA��uA�dZA�/A�/A�
=A�7LA�+A��PA��PA��A�$�A�p�A�`BA�|�A�ZA�A�ZA�1A��DA���A���A�hsA��PA��A��#A�"�A�  A��mA���A�`BA�ZA�O�A���A��DA�Q�A��yA�|�A���A�l�A��A� �A��A�=qA�n�A�&�A���A��`A�r�A�33A��;A�A�bNA�VA�n�A~M�A|�jA|n�A{�#A{S�A{&�Ay�PAr��Ap�\AoAm��Al9XAk+Ai/Ag��Afz�Ad�AbjA`Q�A_?}A^�RA]S�A[C�AW�
AT�`AR��AQ�7AP5?AM�-AL �AKhsAJv�AI�TAI\)AHZAG�
AF�AD�ABbA?`BA<n�A;?}A:{A9oA8$�A6Q�A4��A3O�A1�^A0 �A.�A.�A,��A,E�A)�;A(��A'O�A&Q�A%
=A%�A$1A#�A"bA!��A z�A�7A��A=qA?}A��A�+A��AS�A�uAƨA�#Ax�A?}AM�AhsA��AoA�/A�uAE�A1A?}A�wA��A�
AVAJA
��A
�A
{A
{A	��A	XAĜA��A;dAjA�A
=A��A�wA?}A�RA=qA1'AJA�mA��A|�A @��!@�{@�/@��@��^@�v�@�@��-@��@��`@��@�@��y@蛦@�l�@�R@�{@��#@�/@�%@��
@�C�@�+@�E�@��@��#@�Ĝ@���@���@���@ؓu@�v�@��@��#@�/@�r�@�K�@��#@�9X@��@ˍP@�=q@��@ɲ-@�x�@��;@�\)@�v�@��T@���@ēu@�1'@��@�;d@�{@�b@��@���@��P@�dZ@��@�v�@�hs@�S�@�=q@���@�@��-@�%@�Q�@���@��!@�p�@�&�@��@�1@��H@�$�@���@���@�z�@�j@�bN@�A�@� �@���@�C�@��y@�V@��@�G�@��@��D@��@���@�5?@���@�I�@���@��@���@�O�@�p�@�x�@�V@�&�@�G�@��@���@��m@�33@��\@�n�@���@�$�@���@��^@�7L@��j@�b@�33@���@�hs@� �@�ƨ@�dZ@�C�@�+@�o@��R@�E�@��@�@��^@���@��7@�`B@�/@�V@��/@��u@�r�@�bN@�I�@��@��@�l�@�K�@�;d@�o@���@���@��@�J@�@�@��7@�x�@�G�@�V@���@��u@�Z@���@��@�33@�"�@�ff@�J@�@���@��7@�G�@��`@��@�(�@���@��w@��w@��F@���@�S�@�C�@�S�@�+@���@��+@�V@�E�@�E�@�=q@��#@��^@��@��@��@�`B@�&�@��@���@��9@�9X@�1@���@��w@��F@��@�|�@��@���@�E�@�-@�{@��T@��7@�G�@�%@�Ĝ@�r�@�A�@�b@���@�o@��y@���@�M�@�M�@�-@���@���@��h@�p�@�V@��j@�z�@�bN@�Z@��D@��9@�Z@���@�  @��@��m@��F@�l�@�^5@��@���@���@�p�@�?}@���@�r�@�@�w@l�@~ff@~{@~$�@}@}O�@}V@|�j@|I�@{ƨ@{�@{�@{t�@{S�@{o@z�\@z�@y�^@yhs@x�`@x�9@xA�@w�@w��@w|�@w+@v�y@v�@v�R@vff@vV@vE�@v5?@v@uO�@t��@tz�@t1@sƨ@s��@st�@s33@r��@r=q@q��@qG�@q&�@p�`@p��@p�u@pr�@pQ�@o�@n��@n��@nv�@n@m�-@mp�@m`B@mO�@m?}@m�@l��@l�/@l�/@l�/@l��@lj@lZ@l9X@k��@k"�@j��@jn�@i��@i%@h�`@h��@hb@g|�@g\)@g+@f�@f��@f$�@eV@d��@d�j@dz�@d�@cS�@b��@b��@bn�@b=q@a�^@aX@a7L@`��@_+@^�y@^�@^ȴ@^��@^5?@]��@]@]p�@]?}@\j@[��@[dZ@[33@["�@Z�@Z-@Y��@Y��@X�9@W�w@W\)@W�@V�y@V��@U��@U�@T��@T�@T�D@T9X@Sƨ@SdZ@R�@R~�@Q��@Q�#@Q��@QX@Q&�@PĜ@P�9@P�9@P�9@P��@P��@P�u@P�@Pr�@PbN@P1'@O�P@O+@Nff@Nff@N��@N�+@NE�@M�T@M`B@L�j@LZ@LI�@L(�@L�@K�m@K��@KdZ@KdZ@KS�@KS�@K33@J�@J^5@J�@JJ@I��@I��@Ix�@H��@Hr�@HbN@H �@G�;@G�w@G�P@F��@E`B@E�@D��@D(�@C�F@CdZ@C@B��@B��@B=q@A�@A��@Ahs@A7L@@��@@Ĝ@A%@@��@@��@@��@@�`@@��@@��@@ �@?|�@?
=@>��@>E�@>@=?}@<�/@<�j@<z�@<j@<9X@;ƨ@;��@;��@;C�@:��@:n�@:�@:�@:J@9�^@9X@9%@8��@8�9@8r�@81'@81'@8  @7�@7�@7�P@7l�@7l�@7K�@7+@7�@7�@6��@6ȴ@65?@5�@5�T@5�h@5�@4�@4��@4��@4z�@49X@3��@3��@3@2��@2��@2�\@2^5@2J@1��@1x�@1x�@1hs@0��@0�u@0Q�@/�@.��@.��@.��@.v�@.5?@.E�@.E�@.5?@-�@-�-@-`B@-/@-V@,�@,�/@,�j@,�j@,�@,�D@,j@,I�@,1@+�m@+�F@+t�@*��@*=q@*-@*-@*�@)��@)��@)��@)&�@)%@(�`@(Ĝ@(��@(�@'�;@'|�@'K�@'
=@&�R@&��@&�+@&V@&5?@%�@%��@%�h@%p�@%O�@%�@%V@$�/@$�D@$9X@$1@#��@#ƨ@#ƨ@#dZ@"�H@"��@"~�@"-@!�@!��@!7L@!&�@ ��@ Ĝ@ r�@ Q�@  �@   @�@|�@\)@�@
=@�@ȴ@V@$�@@�@�h@`B@?}@�@�@��@z�@�@�F@S�@o@�!@~�@J@�#@hs@&�@��@��@Q�@ �@  @�;@��@\)@;d@�@
=@��@�y@�@�R@��@ff@$�@�T@��@?}@�/@��@�D@z�@z�@(�@�
@�
@�F@��@t�@S�@33@��@n�@^5@M�@=q@-@�@�#@hs@�@bN@b@�@�;@�@�P@|�@l�@K�@��@��@v�@$�@@�-@�@p�@`B@/@�/@��@�j@�j@�@�@�@�@��@��@�D@Z@9X@1@�m@�
@ƨ@��@�@dZ@@
�\@
n�@
M�@
�@	��@	��@	�#@	�^@	X@��@�`@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B#�B$�B'�B0!B6FB9XB<jB=qBC�BF�BG�B?}B0!B<jBK�BbNBr�B{�B~�B�-B��B�B�)B�NB�BB1BJB	7B1BJB#�B33B2-B2-B5?B;dB:^B8RB:^BD�BVBT�BO�BA�B8RB,B�B�B+B��B�NBĜB��B��B��B��B��B��B��B�B�LB��B�Bq�Bl�BgmB^5BQ�BO�B=qB'�B�BB
��B
�B
�NB
��B
�dB
�B
�oB
jB
Q�B
=qB
1'B
.B
(�B
#�B
�B
\B	�fB	��B	ȴB	�dB	�B	��B	��B	�\B	�B	x�B	hsB	ZB	Q�B	L�B	B�B	49B	 �B	bB	B��B�B�fB�/B�B��B��B��BǮBĜB�wB�3B�B��B��B�{B�hB�PB�7B�%B� B� By�Bw�Br�Bl�BhsBk�BffB`BB]/B[#B]/BbNB]/BYBVBW
BR�BL�BH�BM�BG�BF�BG�BH�BG�BD�BC�BB�B@�B>wB<jB8RB9XB8RB7LB6FB49B2-B0!B/B/B5?B:^B8RB8RB:^B;dB;dB<jB<jB<jB<jB<jB;dB9XB;dB;dB;dB;dB;dB<jB<jB<jB>wB>wB?}BH�BI�BI�BI�BG�BG�BE�BD�BB�BD�BF�BG�BH�BF�BA�BB�BE�BF�BN�BP�BZB_;Be`BffBe`BdZBdZBiyBl�Bl�Bn�Bn�Bn�Bm�Bn�Bn�Bn�Bo�Bq�Bv�Bv�Bw�Bz�B{�B{�Bz�B� B� B�B�B�%B�%B�%B�%B�B�B�=B�=B�=B�=B�=B�DB�DB�=B�JB�DB�JB�oB��B��B��B��B��B��B��B��B��B�B�3B�FB�wB��BBBÖBÖBȴB��B��B��B��B�B�/B�5B�HB�HB�HB�`B�mB�B�B�B��B��B	B	B		7B	hB	�B	�B	�B	�B	�B	�B	 �B	%�B	&�B	(�B	(�B	'�B	(�B	+B	,B	/B	33B	6FB	<jB	@�B	B�B	C�B	D�B	E�B	F�B	H�B	M�B	N�B	O�B	Q�B	T�B	XB	ZB	^5B	`BB	`BB	aHB	aHB	cTB	cTB	cTB	dZB	ffB	gmB	iyB	k�B	l�B	l�B	n�B	q�B	r�B	s�B	t�B	v�B	w�B	w�B	x�B	y�B	z�B	z�B	|�B	}�B	�B	�B	�B	�B	�B	�%B	�7B	�PB	�VB	�VB	�VB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�FB	�LB	�LB	�LB	�XB	�dB	�dB	�jB	�jB	�jB	�qB	�}B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�5B	�5B	�;B	�BB	�NB	�HB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�fB	�fB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
oB
oB
oB
{B
{B
{B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
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
+B
+B
+B
+B
+B
,B
,B
/B
0!B
1'B
1'B
1'B
0!B
0!B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
H�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
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
M�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
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
S�B
S�B
S�B
T�B
T�B
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
XB
XB
XB
XB
YB
YB
YB
YB
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
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
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
dZB
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
gmB
gmB
hsB
hsB
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
l�B
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
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B#�B$�B'�B0!B6FB9XB<jB=qBC�BF�BG�B?}B0!B<jBK�BbNBr�B{�B~�B�-B��B�B�)B�NB�BB1BJB	7B1BJB#�B33B2-B2-B5?B;dB:^B8RB:^BD�BVBT�BO�BA�B8RB,B�B�B+B��B�NBĜB��B��B��B��B��B��B��B�B�LB��B�Bq�Bl�BgmB^5BQ�BO�B=qB'�B�BB
��B
�B
�NB
��B
�dB
�B
�oB
jB
Q�B
=qB
1'B
.B
(�B
#�B
�B
\B	�fB	��B	ȴB	�dB	�B	��B	��B	�\B	�B	x�B	hsB	ZB	Q�B	L�B	B�B	49B	 �B	bB	B��B�B�fB�/B�B��B��B��BǮBĜB�wB�3B�B��B��B�{B�hB�PB�7B�%B� B� By�Bw�Br�Bl�BhsBk�BffB`BB]/B[#B]/BbNB]/BYBVBW
BR�BL�BH�BM�BG�BF�BG�BH�BG�BD�BC�BB�B@�B>wB<jB8RB9XB8RB7LB6FB49B2-B0!B/B/B5?B:^B8RB8RB:^B;dB;dB<jB<jB<jB<jB<jB;dB9XB;dB;dB;dB;dB;dB<jB<jB<jB>wB>wB?}BH�BI�BI�BI�BG�BG�BE�BD�BB�BD�BF�BG�BH�BF�BA�BB�BE�BF�BN�BP�BZB_;Be`BffBe`BdZBdZBiyBl�Bl�Bn�Bn�Bn�Bm�Bn�Bn�Bn�Bo�Bq�Bv�Bv�Bw�Bz�B{�B{�Bz�B� B� B�B�B�%B�%B�%B�%B�B�B�=B�=B�=B�=B�=B�DB�DB�=B�JB�DB�JB�oB��B��B��B��B��B��B��B��B��B�B�3B�FB�wB��BBBÖBÖBȴB��B��B��B��B�B�/B�5B�HB�HB�HB�`B�mB�B�B�B��B��B	B	B		7B	hB	�B	�B	�B	�B	�B	�B	 �B	%�B	&�B	(�B	(�B	'�B	(�B	+B	,B	/B	33B	6FB	<jB	@�B	B�B	C�B	D�B	E�B	F�B	H�B	M�B	N�B	O�B	Q�B	T�B	XB	ZB	^5B	`BB	`BB	aHB	aHB	cTB	cTB	cTB	dZB	ffB	gmB	iyB	k�B	l�B	l�B	n�B	q�B	r�B	s�B	t�B	v�B	w�B	w�B	x�B	y�B	z�B	z�B	|�B	}�B	�B	�B	�B	�B	�B	�%B	�7B	�PB	�VB	�VB	�VB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�FB	�LB	�LB	�LB	�XB	�dB	�dB	�jB	�jB	�jB	�qB	�}B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�5B	�5B	�;B	�BB	�NB	�HB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�mB	�fB	�fB	�`B	�fB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
oB
oB
oB
{B
{B
{B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
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
+B
+B
+B
+B
+B
,B
,B
/B
0!B
1'B
1'B
1'B
0!B
0!B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
9XB
:^B
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
H�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K�B
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
M�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
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
S�B
S�B
S�B
T�B
T�B
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
XB
XB
XB
XB
YB
YB
YB
YB
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
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
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
dZB
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
gmB
gmB
hsB
hsB
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
l�B
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
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20191231130020                              AO  ARCAADJP                                                                    20191231130020    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191231130020  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191231130020  QCF$                G�O�G�O�G�O�0               