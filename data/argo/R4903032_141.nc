CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-05-14T18:35:59Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20220514183559  20220514183559  4903032 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @��T7_;�1   @��T��� @;ݲ-V�dn��O�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� DafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ D�|�D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D��3D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\*@�{@�{A
=A;
=A[
=A{
=A��A��A��A��AͅA݅A�A��BBBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)�>C+��C-��C/��C1��C3��C5��C7��C9��C;�>C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�)D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8�)D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�)Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxl)Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��GD�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�r�DǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�2�D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�6D�vDչGD��D�6D�vDֶD��D�6D�vD׶D��D�6D�vDضD��D�6D�vDٶD��D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�vDݶD��D�6D�vD޶D��D�6D�vD߶D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�yGD��D��D�6D�vD��D��D�6D�vD��D��GD�&1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��!A��9A��RA��RA��FA���A��A�XA�O�A�?}A�9XA�33A�/A�$�A���A�S�A�ZA���A�A�?}A���A�  A�A�33A��7A���A���A�7LA���A��A�oA��\A�  A��A���A�VA�VA���A�(�A��9A���A��hA�M�A�XA�XA�A�33A��uA��A���A���A�K�A�JA���A�ƨA��mA��!A�n�A���A��-A�ȴA�1'A� �A�VA�A��`A���A�|�A�G�A�%A�?}A��A��A��HA�z�A�ĜA�bA�9XA���A��7A��-A�jA��TA�(�A��A��A�A���A��hA��A�$�A�|�A�?}A��A�`BA��jA�^5AoA}�A|JAyC�Aw�hAvQ�Au\)As7LAp��Am��Ak%Aj9XAi�AgC�AfI�AbI�A`�HA]�wA\-AZ�AY��AXM�AW33AT�`ASC�AQ33AP1AO�AO33AN�AL��AK�mAK�wAK+AIx�AH5?AGS�AFJAE`BADz�ADA�ADJAC�A@5?A=|�A<9XA:��A9��A8��A7hsA6�!A6��A6�+A5�mA4^5A4JA3��A3hsA3&�A2ȴA1�hA0n�A/�TA.�A.�A.�uA.{A-��A-dZA,�A*��A(ĜA'��A'&�A&��A&bA%��A$�9A$$�A#�A"9XA!l�A -AoA�jA~�AffAbA��A�#A~�AZA�/A�DA��AhsA�yAA�A�A�jA��A\)A�HA~�A�A��An�A�FA/A�A��AI�A�#A�A7LA
�yA	�
A	;dAI�Al�A�A5?A��AAVAJAXA��A  A��A�^A��A�PAhsA �yA ff@��m@�
=@��@��-@���@���@�E�@��@�O�@�&�@��m@��-@땁@�~�@�E�@��@���@�5?@�A�@�-@�ȴ@��`@ܓu@�bN@�(�@���@�\)@��H@�^5@٩�@���@���@�l�@�"�@�ff@Ձ@�;d@�r�@�|�@�n�@�r�@�=q@�?}@ȴ9@�b@�\)@���@�x�@�9X@�ƨ@Õ�@�"�@§�@�~�@�n�@�M�@���@���@�K�@�^5@�7L@��@���@�Q�@���@���@��+@��@���@�Q�@���@���@��;@�|�@�+@�V@�@��7@��`@��F@�t�@�S�@��@�r�@� �@��m@��w@�l�@�E�@��@�Z@���@��w@�l�@�33@��y@�=q@�p�@�?}@��@��D@���@�J@�&�@��D@�j@�1'@�1@���@�C�@���@��@�ȴ@��!@�E�@��h@��@��9@�bN@�I�@�9X@�(�@�  @���@�t�@��\@�&�@���@�j@�1'@�(�@� �@�1@���@�+@��H@���@�v�@�M�@�-@�J@��^@�O�@��j@� �@��w@���@���@�~�@�V@�{@��@�O�@�j@��@��;@��@�n�@�J@�@���@�x�@�X@�O�@�%@��`@��/@�A�@��@�b@�b@���@��P@�K�@�o@��@��y@��H@��+@�M�@�$�@���@�x�@�O�@��@�V@���@���@�j@�b@��;@�dZ@���@�5?@�@��@��-@��@�`B@�?}@��`@���@�Z@�w@~�@~�R@~ff@}�@|�@|z�@|�@{��@{o@z��@zJ@yx�@y%@xĜ@xr�@x1'@w�@w��@w�@v��@vff@vV@v5?@up�@t�@t(�@s33@r�@r�H@r��@r�!@rn�@q�^@q&�@pbN@o�w@o�@nȴ@n��@nE�@n$�@n@m�h@mO�@m/@m/@m�@l��@l��@lI�@k�
@k��@kt�@k"�@ko@j�@j��@j~�@j=q@jJ@jJ@jJ@i��@i�@i�@i�#@i��@i��@i7L@h��@h�u@hbN@h1'@g�;@g\)@g+@f�y@fȴ@f�R@f�+@f5?@f5?@f$�@e@e`B@d��@d��@dZ@c�F@c@b�!@b��@b�\@bn�@bM�@bM�@bM�@bM�@b=q@b-@b�@b�@b�@b�@b�@a��@a��@aG�@`�`@`�u@`Q�@`A�@`1'@`b@_��@_+@_�@^�R@^��@^��@^�+@^v�@^$�@]p�@\j@[��@Z~�@ZJ@Y�^@Yx�@Y&�@Xr�@W��@W+@W+@W+@W�@W
=@V�y@Vȴ@V�@V�@Vȴ@Vȴ@V��@V�+@V�+@V{@U�T@U�@TZ@T1@S��@S33@R��@Rn�@Qhs@Q&�@Q%@P�`@Pr�@Pb@O�;@O|�@O;d@N��@Nff@NV@N{@N@M�T@M��@M@M��@M`B@MV@L��@LZ@K�
@KS�@K@K@J�@J��@J�!@J�\@JM�@I�@I�^@I�@H�@G��@GK�@G;d@G
=@F��@F��@Fȴ@FV@F$�@E��@Ep�@E?}@D�@D�@Dz�@DZ@C��@C�@C33@B~�@A��@A��@A�7@A%@@Ĝ@@�u@@bN@@A�@@1'@@  @?�@?�;@?�;@?��@?�w@?��@?l�@?K�@?
=@>��@>v�@>@=�-@=/@<�j@<�@<j@<(�@<9X@<(�@;��@;��@;��@;��@;��@;�m@;�F@;��@;dZ@:��@:=q@9�@9��@9�^@9�7@8��@8��@8��@8�u@8�u@8r�@8bN@8Q�@7�@7�@6�R@6ff@6@5O�@5/@5�@5V@4�j@4�D@4Z@4�@3�m@3��@3C�@3"�@3@2�H@2��@2��@2��@2�\@2�\@2~�@2n�@2M�@2J@1��@1�^@1��@1��@1�7@1X@1�@0��@0��@0A�@/�@/�@/�P@/;d@.�@.ȴ@.��@.V@.5?@.{@-�@-��@-��@-p�@-V@,z�@,1@+�
@+ƨ@+t�@+dZ@+S�@*�\@*-@)��@)�#@)�^@)�7@)x�@)G�@(Ĝ@(�@(r�@(bN@(bN@(bN@(Q�@(b@'l�@'+@'
=@&��@&��@&V@&5?@%`B@$��@$z�@$Z@$I�@$(�@$1@#��@#�
@#��@#��@#��@#�@#t�@#t�@#dZ@#S�@#33@#"�@#@"�H@"��@"~�@"^5@"J@!�7@!�@ ��@ Ĝ@ �9@ �9@ ��@ �u@ bN@�@\)@;d@+@�@��@ff@{@��@�h@�@O�@�D@j@Z@Z@I�@(�@(�@�@1@1@��@�m@ƨ@�F@��@��@��@�@t�@C�@o@�H@^5@J@��@�7@&�@��@�9@�@A�@�@�;@�;@�;@��@�w@�P@�P@|�@|�@|�@|�@l�@l�@l�@\)@;d@;d@;d@;d@+@�@��@V@$�@�@@?}@?}@/@/@/@/@/@/@/@��@�D@I�@9X@(�@(�@(�@�
@dZ@"�@�@�H@�!@��@~�@M�@�@G�@&�@��@�9@��@r�@A�@1'@ �@ �@  @�@\)@��@�@v�@$�@{@�@�T@��@@�-@��@�h@p�@O�@?}@�@�@V@��@j@j@Z@I�@9X@(�@(�@�@�@1@�m@ƨ@�@t�@S�@"�@o@@
�@
�H@
��@
��@
�!@
^5@
=q@
-@
J@	��@	��@	��@	�#@	��@	��@	hs@	G�@	&�@	%@��@�9@�u@r�@bN@��@l�@\)@\)@K�@;d@�@�y@�R@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��!A��9A��RA��RA��FA���A��A�XA�O�A�?}A�9XA�33A�/A�$�A���A�S�A�ZA���A�A�?}A���A�  A�A�33A��7A���A���A�7LA���A��A�oA��\A�  A��A���A�VA�VA���A�(�A��9A���A��hA�M�A�XA�XA�A�33A��uA��A���A���A�K�A�JA���A�ƨA��mA��!A�n�A���A��-A�ȴA�1'A� �A�VA�A��`A���A�|�A�G�A�%A�?}A��A��A��HA�z�A�ĜA�bA�9XA���A��7A��-A�jA��TA�(�A��A��A�A���A��hA��A�$�A�|�A�?}A��A�`BA��jA�^5AoA}�A|JAyC�Aw�hAvQ�Au\)As7LAp��Am��Ak%Aj9XAi�AgC�AfI�AbI�A`�HA]�wA\-AZ�AY��AXM�AW33AT�`ASC�AQ33AP1AO�AO33AN�AL��AK�mAK�wAK+AIx�AH5?AGS�AFJAE`BADz�ADA�ADJAC�A@5?A=|�A<9XA:��A9��A8��A7hsA6�!A6��A6�+A5�mA4^5A4JA3��A3hsA3&�A2ȴA1�hA0n�A/�TA.�A.�A.�uA.{A-��A-dZA,�A*��A(ĜA'��A'&�A&��A&bA%��A$�9A$$�A#�A"9XA!l�A -AoA�jA~�AffAbA��A�#A~�AZA�/A�DA��AhsA�yAA�A�A�jA��A\)A�HA~�A�A��An�A�FA/A�A��AI�A�#A�A7LA
�yA	�
A	;dAI�Al�A�A5?A��AAVAJAXA��A  A��A�^A��A�PAhsA �yA ff@��m@�
=@��@��-@���@���@�E�@��@�O�@�&�@��m@��-@땁@�~�@�E�@��@���@�5?@�A�@�-@�ȴ@��`@ܓu@�bN@�(�@���@�\)@��H@�^5@٩�@���@���@�l�@�"�@�ff@Ձ@�;d@�r�@�|�@�n�@�r�@�=q@�?}@ȴ9@�b@�\)@���@�x�@�9X@�ƨ@Õ�@�"�@§�@�~�@�n�@�M�@���@���@�K�@�^5@�7L@��@���@�Q�@���@���@��+@��@���@�Q�@���@���@��;@�|�@�+@�V@�@��7@��`@��F@�t�@�S�@��@�r�@� �@��m@��w@�l�@�E�@��@�Z@���@��w@�l�@�33@��y@�=q@�p�@�?}@��@��D@���@�J@�&�@��D@�j@�1'@�1@���@�C�@���@��@�ȴ@��!@�E�@��h@��@��9@�bN@�I�@�9X@�(�@�  @���@�t�@��\@�&�@���@�j@�1'@�(�@� �@�1@���@�+@��H@���@�v�@�M�@�-@�J@��^@�O�@��j@� �@��w@���@���@�~�@�V@�{@��@�O�@�j@��@��;@��@�n�@�J@�@���@�x�@�X@�O�@�%@��`@��/@�A�@��@�b@�b@���@��P@�K�@�o@��@��y@��H@��+@�M�@�$�@���@�x�@�O�@��@�V@���@���@�j@�b@��;@�dZ@���@�5?@�@��@��-@��@�`B@�?}@��`@���@�Z@�w@~�@~�R@~ff@}�@|�@|z�@|�@{��@{o@z��@zJ@yx�@y%@xĜ@xr�@x1'@w�@w��@w�@v��@vff@vV@v5?@up�@t�@t(�@s33@r�@r�H@r��@r�!@rn�@q�^@q&�@pbN@o�w@o�@nȴ@n��@nE�@n$�@n@m�h@mO�@m/@m/@m�@l��@l��@lI�@k�
@k��@kt�@k"�@ko@j�@j��@j~�@j=q@jJ@jJ@jJ@i��@i�@i�@i�#@i��@i��@i7L@h��@h�u@hbN@h1'@g�;@g\)@g+@f�y@fȴ@f�R@f�+@f5?@f5?@f$�@e@e`B@d��@d��@dZ@c�F@c@b�!@b��@b�\@bn�@bM�@bM�@bM�@bM�@b=q@b-@b�@b�@b�@b�@b�@a��@a��@aG�@`�`@`�u@`Q�@`A�@`1'@`b@_��@_+@_�@^�R@^��@^��@^�+@^v�@^$�@]p�@\j@[��@Z~�@ZJ@Y�^@Yx�@Y&�@Xr�@W��@W+@W+@W+@W�@W
=@V�y@Vȴ@V�@V�@Vȴ@Vȴ@V��@V�+@V�+@V{@U�T@U�@TZ@T1@S��@S33@R��@Rn�@Qhs@Q&�@Q%@P�`@Pr�@Pb@O�;@O|�@O;d@N��@Nff@NV@N{@N@M�T@M��@M@M��@M`B@MV@L��@LZ@K�
@KS�@K@K@J�@J��@J�!@J�\@JM�@I�@I�^@I�@H�@G��@GK�@G;d@G
=@F��@F��@Fȴ@FV@F$�@E��@Ep�@E?}@D�@D�@Dz�@DZ@C��@C�@C33@B~�@A��@A��@A�7@A%@@Ĝ@@�u@@bN@@A�@@1'@@  @?�@?�;@?�;@?��@?�w@?��@?l�@?K�@?
=@>��@>v�@>@=�-@=/@<�j@<�@<j@<(�@<9X@<(�@;��@;��@;��@;��@;��@;�m@;�F@;��@;dZ@:��@:=q@9�@9��@9�^@9�7@8��@8��@8��@8�u@8�u@8r�@8bN@8Q�@7�@7�@6�R@6ff@6@5O�@5/@5�@5V@4�j@4�D@4Z@4�@3�m@3��@3C�@3"�@3@2�H@2��@2��@2��@2�\@2�\@2~�@2n�@2M�@2J@1��@1�^@1��@1��@1�7@1X@1�@0��@0��@0A�@/�@/�@/�P@/;d@.�@.ȴ@.��@.V@.5?@.{@-�@-��@-��@-p�@-V@,z�@,1@+�
@+ƨ@+t�@+dZ@+S�@*�\@*-@)��@)�#@)�^@)�7@)x�@)G�@(Ĝ@(�@(r�@(bN@(bN@(bN@(Q�@(b@'l�@'+@'
=@&��@&��@&V@&5?@%`B@$��@$z�@$Z@$I�@$(�@$1@#��@#�
@#��@#��@#��@#�@#t�@#t�@#dZ@#S�@#33@#"�@#@"�H@"��@"~�@"^5@"J@!�7@!�@ ��@ Ĝ@ �9@ �9@ ��@ �u@ bN@�@\)@;d@+@�@��@ff@{@��@�h@�@O�@�D@j@Z@Z@I�@(�@(�@�@1@1@��@�m@ƨ@�F@��@��@��@�@t�@C�@o@�H@^5@J@��@�7@&�@��@�9@�@A�@�@�;@�;@�;@��@�w@�P@�P@|�@|�@|�@|�@l�@l�@l�@\)@;d@;d@;d@;d@+@�@��@V@$�@�@@?}@?}@/@/@/@/@/@/@/@��@�D@I�@9X@(�@(�@(�@�
@dZ@"�@�@�H@�!@��@~�@M�@�@G�@&�@��@�9@��@r�@A�@1'@ �@ �@  @�@\)@��@�@v�@$�@{@�@�T@��@@�-@��@�h@p�@O�@?}@�@�@V@��@j@j@Z@I�@9X@(�@(�@�@�@1@�m@ƨ@�@t�@S�@"�@o@@
�@
�H@
��@
��@
�!@
^5@
=q@
-@
J@	��@	��@	��@	�#@	��@	��@	hs@	G�@	&�@	%@��@�9@�u@r�@bN@��@l�@\)@\)@K�@;d@�@�y@�R@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�DB�B�B}�Bz�Bz�Bx�Bv�Bt�Br�Bp�Bl�BiyBffBbNB_;BZBP�B?}B<jB9XB6FB0!B(�B�BoB	7BB��B�B�B�TB��B��B��BɺBǮB�jB�LB�3B�B��B�VB�BjBP�B=qB.B&�B$�B �B�BoB�B�TB�5B�B��BĜB�FB�B�bBp�B\)BQ�BD�B49B�B{B\BJB+B��B�B�B�mB�NB�#B�B��BŢB��B�RB�B��B��B�\B~�Bl�B\)BT�BM�BC�B=qB!�BoB%B��B�B�B�HB�)B�
B��B�wB�?B�'B�B��B��B��B��B��B�\B�B}�Bz�Bw�Bs�Br�Bp�Bm�B`BB\)B]/BYBT�BS�BS�BP�BO�BN�BM�BH�BF�BD�BA�B>wB<jB:^B7LB6FB49B5?B5?B5?B2-B/B)�B&�B�B�B�B�B�B�B�BuBuBuBbBuBhBuBoBhBoBbBPB
=B+BBBB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�yB
�yB
�sB
�sB
�mB
�fB
�fB
�fB
�fB
�fB
�`B
�TB
�BB
�#B
�B
�B
�B
�B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�#B
�)B
�)B
�#B
�#B
�)B
�/B
�5B
�)B
�)B
�;B
�HB
�`B
�sB
�mB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B  BBB%B+B+B
=BbBoBoBuB�B�B%�B'�B(�B)�B,B-B.B2-B6FB7LB9XB:^B@�BI�BM�BQ�BR�BR�BS�BW
BYBZB[#B\)B\)B^5BcTBe`BhsBjBk�Bk�Bk�Bl�Bm�Bn�Br�By�B}�B� B�B�B�B�B�+B�DB�PB�\B�bB�hB�oB�uB��B��B��B��B��B��B��B�B�B�'B�9B�^B��BĜBĜBǮB��B��B�B�B�B�B�B�)B�/B�/B�TB�ZB�ZB�ZB�fB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B  BBB1B	7BPBoB�B�B�B�B�B�B�B!�B#�B%�B)�B.B/B0!B49B6FB8RB9XB;dB>wB@�BC�BF�BH�BI�BJ�BK�BL�BN�BP�BR�BT�BT�BVBYB[#B]/BbNBdZBdZBe`Be`BffBiyBk�Bn�Bp�Bs�Bt�Bu�Bv�Bw�Bx�Bz�B{�B|�B|�B|�B~�B~�B�B�B�B�B�%B�%B�+B�1B�7B�=B�DB�DB�DB�DB�JB�JB�JB�PB�VB�bB�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�'B�3B�9B�?B�?B�FB�FB�RB�XB�XB�dB�dB�dB�dB�dB�qB�}BBŢBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�
B�B�)B�/B�5B�;B�BB�HB�ZB�ZB�`B�`B�fB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBB%B1B1B1B
=B
=BDBDBDBJBJBJBJBJBJBPBPBPBVBVB\B\BbBbBhBoBoBuBuBuBuB{B{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B!�B!�B!�B"�B"�B#�B#�B#�B$�B$�B$�B$�B%�B%�B%�B%�B%�B%�B&�B&�B&�B&�B'�B'�B'�B(�B(�B(�B)�B)�B+B+B,B,B,B-B-B-B.B.B.B.B/B/B0!B1'B1'B1'B2-B2-B2-B49B49B5?B5?B5?B5?B5?B6FB7LB7LB7LB7LB7LB7LB7LB8RB9XB9XB:^B:^B:^B;dB;dB<jB=qB>wB>wB>wB>wB?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B@�B@�B@�BA�BA�BA�BA�BB�BB�BC�BC�BD�BD�BD�BD�BD�BD�BE�BE�BF�BG�BG�BG�BG�BH�BH�BI�BI�BI�BI�BK�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BM�BM�BM�BM�BM�BM�BM�BM�BN�BO�BO�BO�BP�BP�BQ�BQ�BR�BR�BS�BS�BS�BS�BS�BS�BS�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BVBVBVBVBW
BW
BW
BXBYBYBYBYBYBYBYBYBYBYBZBZB[#B[#B[#B[#B[#B\)B\)B]/B]/B]/B]/B]/B]/B^5B_;B_;B`BB`BB`BB`BBaHBaHBaHBaHBaHBaHBbNBcTBcTBcTBdZBdZBdZBe`Be`Be`Be`Be`Be`Be`BffBffBffBffBffBgmBgmBgmBgmBhsBhsBhsBhsBhsBhsBhsBhsBiyBiyBiyBiyBjBjBjBjBjBjBjBk�Bk�Bk�Bk�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bq�Br�Br�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�DB�B�B}�Bz�Bz�Bx�Bv�Bt�Br�Bp�Bl�BiyBffBbNB_;BZBP�B?}B<jB9XB6FB0!B(�B�BoB	7BB��B�B�B�TB��B��B��BɺBǮB�jB�LB�3B�B��B�VB�BjBP�B=qB.B&�B$�B �B�BoB�B�TB�5B�B��BĜB�FB�B�bBp�B\)BQ�BD�B49B�B{B\BJB+B��B�B�B�mB�NB�#B�B��BŢB��B�RB�B��B��B�\B~�Bl�B\)BT�BM�BC�B=qB!�BoB%B��B�B�B�HB�)B�
B��B�wB�?B�'B�B��B��B��B��B��B�\B�B}�Bz�Bw�Bs�Br�Bp�Bm�B`BB\)B]/BYBT�BS�BS�BP�BO�BN�BM�BH�BF�BD�BA�B>wB<jB:^B7LB6FB49B5?B5?B5?B2-B/B)�B&�B�B�B�B�B�B�B�BuBuBuBbBuBhBuBoBhBoBbBPB
=B+BBBB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�yB
�yB
�sB
�sB
�mB
�fB
�fB
�fB
�fB
�fB
�`B
�TB
�BB
�#B
�B
�B
�B
�B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�#B
�)B
�)B
�#B
�#B
�)B
�/B
�5B
�)B
�)B
�;B
�HB
�`B
�sB
�mB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B  BBB%B+B+B
=BbBoBoBuB�B�B%�B'�B(�B)�B,B-B.B2-B6FB7LB9XB:^B@�BI�BM�BQ�BR�BR�BS�BW
BYBZB[#B\)B\)B^5BcTBe`BhsBjBk�Bk�Bk�Bl�Bm�Bn�Br�By�B}�B� B�B�B�B�B�+B�DB�PB�\B�bB�hB�oB�uB��B��B��B��B��B��B��B�B�B�'B�9B�^B��BĜBĜBǮB��B��B�B�B�B�B�B�)B�/B�/B�TB�ZB�ZB�ZB�fB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B  BBB1B	7BPBoB�B�B�B�B�B�B�B!�B#�B%�B)�B.B/B0!B49B6FB8RB9XB;dB>wB@�BC�BF�BH�BI�BJ�BK�BL�BN�BP�BR�BT�BT�BVBYB[#B]/BbNBdZBdZBe`Be`BffBiyBk�Bn�Bp�Bs�Bt�Bu�Bv�Bw�Bx�Bz�B{�B|�B|�B|�B~�B~�B�B�B�B�B�%B�%B�+B�1B�7B�=B�DB�DB�DB�DB�JB�JB�JB�PB�VB�bB�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�'B�3B�9B�?B�?B�FB�FB�RB�XB�XB�dB�dB�dB�dB�dB�qB�}BBŢBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�
B�B�)B�/B�5B�;B�BB�HB�ZB�ZB�`B�`B�fB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBB%B1B1B1B
=B
=BDBDBDBJBJBJBJBJBJBPBPBPBVBVB\B\BbBbBhBoBoBuBuBuBuB{B{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B!�B!�B!�B"�B"�B#�B#�B#�B$�B$�B$�B$�B%�B%�B%�B%�B%�B%�B&�B&�B&�B&�B'�B'�B'�B(�B(�B(�B)�B)�B+B+B,B,B,B-B-B-B.B.B.B.B/B/B0!B1'B1'B1'B2-B2-B2-B49B49B5?B5?B5?B5?B5?B6FB7LB7LB7LB7LB7LB7LB7LB8RB9XB9XB:^B:^B:^B;dB;dB<jB=qB>wB>wB>wB>wB?}B?}B?}B?}B?}B?}B@�B@�B@�B@�B@�B@�B@�BA�BA�BA�BA�BB�BB�BC�BC�BD�BD�BD�BD�BD�BD�BE�BE�BF�BG�BG�BG�BG�BH�BH�BI�BI�BI�BI�BK�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BM�BM�BM�BM�BM�BM�BM�BM�BN�BO�BO�BO�BP�BP�BQ�BQ�BR�BR�BS�BS�BS�BS�BS�BS�BS�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BT�BVBVBVBVBW
BW
BW
BXBYBYBYBYBYBYBYBYBYBYBZBZB[#B[#B[#B[#B[#B\)B\)B]/B]/B]/B]/B]/B]/B^5B_;B_;B`BB`BB`BB`BBaHBaHBaHBaHBaHBaHBbNBcTBcTBcTBdZBdZBdZBe`Be`Be`Be`Be`Be`Be`BffBffBffBffBffBgmBgmBgmBgmBhsBhsBhsBhsBhsBhsBhsBhsBiyBiyBiyBiyBjBjBjBjBjBjBjBk�Bk�Bk�Bk�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bn�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bq�Br�Br�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220514183559                              AO  ARCAADJP                                                                    20220514183559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220514183559  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220514183559  QCF$                G�O�G�O�G�O�8000            