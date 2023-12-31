CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-06-15T17:02:44Z creation      
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
resolution        =���   axis      Z        t  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  `   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     t  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180615170244  20180615170244  5904057 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5006                            2B  A   NAVIS_A                         0305                            082713                          863 @�j���v�1   @�j�[�`@6��hr��d�$�/1   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A���A���A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dxy�Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @r�\@�{@�{A
=A;
=A[
=A{
=A��A�Q�A�Q�A��AͅAܸRA�A��BBBBB&B.B6B>BFBNBVB^BfBnBvB~B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHC��C��C��C��C	��C��C��C��C��C�>C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�˅C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD l)D �)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D	l)D	�)D
l)D
�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D��Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)Dl)D�)D l)D �)D!l)D!�)D"l)D"�)D#l)D#�)D$l)D$�)D%l)D%�)D&l)D&�)D'l)D'�)D(l)D(�)D)l)D)�)D*l)D*�)D+l)D+�)D,l)D,�D-l)D-�)D.l)D.�)D/l)D/�)D0l)D0�)D1l)D1�)D2l)D2�)D3l)D3�)D4l)D4�)D5l)D5�)D6l)D6�)D7l)D7�)D8l)D8��D9l)D9�)D:l)D:�)D;l)D;�)D<l)D<�)D=l)D=�)D>l)D>�)D?l)D?�)D@l)D@�)DAl)DA�)DBl)DB�)DCl)DC�)DDl)DD�)DEl)DE�)DFl)DF�)DGl)DG�)DHl)DH�)DIl)DI�)DJl)DJ�)DKl)DK�)DLl)DL�)DMl)DM�)DNl)DN�)DOl)DO�)DPl)DP�)DQl)DQ�)DRl)DR�)DSl)DS�)DTl)DT�)DUl)DU�)DVl)DV�)DWl)DW�)DXl)DX�)DYl)DY�)DZl)DZ�)D[l)D[�)D\l)D\�)D]l)D]�)D^l)D^�)D_l)D_�)D`l)D`�)Dal)Da�)Dbl)Db�)Dcl)Dc�)Ddl)Dd�)Del)De�)Dfl)Df�)Dgl)Dg�)Dhl)Dh�)Dil)Di�)Djl)Dj�Dkl)Dk�)Dll)Dl�)Dml)Dm�)Dnl)Dn�)Dol)Do�)Dpl)Dp�)Dql)Dq�)Drl)Dr�)Dsl)Ds�)Dtl)Dt�)Dul)Du�)Dvl)Dv�)Dwl)Dw�)Dxe�Dx�)Dyl)Dy�)Dzl)Dz�)D{l)D{�)D|l)D|�)D}l)D}�)D~l)D~�)Dl)D�)D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�2�D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�yGD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�yGD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD���D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D���D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD��D��D�6D�vD¶D��D�6D�vDöD��D�6D�vDĶD��D�6D�vDŶD��D�6D�vDƶD��D�6D�vDǶD��D�6D�vDȶD��D�6D�vDɶD��D�6D�vDʶD��D�6D�vD˶D��D�6D�vD̶D��D�6D�vDͶD��D�6D�vDζD��D�6D�vD϶D��D�6D�vDжD��D�6D�vDѶD��D�6D�vDҶD��D�6D�vDӶD��D�6D�vDԶD��D�6D�vDնD��D�6D�vDֶD��D�6D�vD׶D��D�6D�vDضD��D�6D�vDٶD��D�6D�vDڶD��D�6D�vD۶D��D�6D�vDܶD��D�6D�vDݶD��D�6D�vD޶D��D�6D�vD߶D��D�6D�vD�GD��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD��D��D�6D�vD�D��D�6D�vD�D���D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�vD�D��D�6D�r�D��D��D�6D�vD��D��D�6D�vD��D��D�9GD�r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A�r�A�ZA���A�jA�Q�A�bNA��A̛�A�n�A�$�A�ĜA�jA���A�=qA�|�A�\)A�1A��7A��A��7A���A�33A�p�A���A��HA���A��A�Q�A���A��A� �A��\A�ZA�A�1'A�|�A�%A��!A�C�A�-A���A��PA�jA�|�A�+A��A�ƨA���A�dZA���A�v�A�I�A�7LA���A�\)A�"�A��^A�VA�A�ĜA��hA�`BA��A��`A�ƨA��-A�A�A��hA�ZA��-A��FA�|�A��9A��A��A��PA�9XA�ZA��
A���A�p�A�%A�ffA�33A�A�bNA�XA��wA�A��7A�7LA���A��A�ȴA��wA��-A��FA��A�x�A�`BA�G�A�-A��A���A���A��A��A��A�33A��HA�\)A�  A�"�A�FA�hA�A~�A|��Azv�Ax=qAw�Av�Au/At9XAr�uAp��Ap{AnbNAmVAk�PAi/Ag�Ae�PAeoAd��AcƨAb�jAb{AaA]��AZ��AY�FAYl�AYdZAY\)AY?}AY;dAY&�AX�AW33AU33ATv�AS�AR�APbNAOdZAL�AKdZAJĜAH��AF�AF^5AE��AE"�AD~�AD(�AC�hAAƨA@�DA?�
A?"�A>A�A<��A;��A;�7A;XA:1'A8��A7l�A7?}A6�DA6�A57LA3�mA1�hA1
=A0�A09XA/��A.��A-�A-`BA-
=A+��A+|�A+;dA*��A)�PA(�A(ZA(=qA(�A'�mA'&�A&~�A&bA%�FA%;dA$��A#�#A#VA"��A"�+A!?}A��A�A��A+A-A��AA��AoAXA~�A1'AS�A�!A�AJA~�AC�A��A	ƨA��A��A��A��A��A�
AdZAK�A"�A^5A�A�#At�A b@�J@�z�@���@��@���@�@�n�@�|�@���@�&�@�dZ@�hs@�@�  @�\)@�-@�@۝�@���@���@ׅ@�@�?}@�  @�|�@�dZ@�K�@�n�@���@�|�@ϝ�@�|�@�V@���@ɉ7@�bN@�(�@��
@ǅ@�dZ@�"�@�ff@��@�r�@�ƨ@�t�@�
=@�ff@���@�j@�b@��H@�-@�{@�$�@�n�@�^5@��-@���@�;d@�~�@�v�@�n�@�V@�r�@���@�?}@��@��j@��u@�9X@���@���@��^@��j@�(�@��@���@��7@�7L@��@�O�@�G�@��@��j@�9X@�\)@�"�@��!@��+@�=q@���@�G�@��@�b@��@��w@��@�ff@���@��-@��@���@�r�@�Q�@�(�@��@��@�|�@���@��@�&�@�z�@�b@���@�K�@�@���@�~�@��^@�G�@�7L@��j@�Q�@�I�@� �@�b@�ƨ@��@�t�@�C�@��@���@���@�V@��@�7L@�%@���@���@���@�j@�1'@��;@�l�@���@���@��h@�x�@�&�@��@�I�@� �@�A�@��
@�|�@�+@��y@��R@��\@�V@�$�@��#@���@�G�@�%@��9@��u@�j@�A�@�1'@�b@�1@�1@��;@�ƨ@��w@���@�dZ@�;d@��@�M�@���@���@��@���@��@�r�@�j@�I�@� �@���@�;d@��H@�~�@�v�@�{@��T@���@�`B@�&�@��j@�A�@��m@���@�l�@�C�@�"�@��y@�^5@�J@��-@��@�/@��`@���@�r�@�Z@�Q�@�Q�@��;@��@�C�@��@��y@���@���@�v�@�5?@���@��-@���@��7@�X@��@���@��@���@��u@��D@��@�I�@�1@�;@�w@�w@�@�@�@|�@~��@~��@~ff@~@}�-@}�h@}O�@|��@|�j@|(�@{"�@z�@z�\@z�@y�^@y��@yhs@x�`@y�@y&�@x�@w�@wl�@w;d@v�y@vff@u��@uO�@t��@tj@tj@tI�@t9X@t�@s��@s�
@s�F@r�!@q�#@q��@q7L@p�`@pr�@o�@o��@o�@n��@nV@m?}@l�D@lj@lZ@l9X@l�@l�@l�@l1@k�
@kt�@k33@k"�@k@j��@j�\@jM�@i��@i%@h��@hr�@hA�@g�@g�@g
=@e��@eO�@eV@dI�@c�m@c��@c��@b�\@b-@a��@ahs@`Q�@`1'@`1'@`b@_�;@_�w@_�@_�@_�@_��@_�P@_K�@^V@^@]�@]��@]�-@]�h@]�h@]/@\�j@\j@\(�@\�@[�
@[�@[C�@Z�H@Z~�@Z^5@ZJ@Y�@Y��@Y��@YX@Y&�@X��@X��@W�@Vff@U�@U��@U�-@U��@U�h@U�@T�j@T�j@Tz�@T1@S��@SS�@R�@R�\@RJ@Q��@Q&�@P��@PĜ@PbN@O|�@Nȴ@N�+@NE�@M�@M/@L�j@K��@Kƨ@KS�@Ko@J��@J^5@I��@I��@Ihs@I&�@I%@H�u@G�;@G�@G��@G��@Gl�@G\)@G;d@G
=@Fȴ@F��@F��@Fv�@FE�@F@E@E�@EO�@EV@Dj@C��@C��@CS�@C"�@B�H@B�\@A�7@AG�@A&�@@��@@��@@��@@bN@@A�@?�@?l�@?
=@>v�@=�T@=�h@=?}@=/@=/@<�/@<9X@<1@;�m@;�F@;S�@:��@:=q@:�@9�@9��@9G�@9%@8�u@7�@7��@7�@7|�@7K�@7+@7+@7+@7+@7+@7+@7+@7
=@7
=@6�y@6�@6$�@5�@4�/@4�/@4�/@4��@4��@4�@4�D@49X@3�
@3��@3��@3t�@3C�@333@3"�@3o@2�@2�H@2�H@2�H@2�\@2^5@2M�@2M�@2-@2-@2-@2J@1�#@1�^@1�7@0Ĝ@0Ĝ@0��@01'@/��@/|�@/l�@/\)@/+@/+@/�@.�R@-�T@-`B@-/@-�@-�@-V@,�@,��@,�D@,�D@,z�@,Z@,I�@,(�@+�
@+t�@+dZ@+S�@+S�@+"�@*�\@*M�@)�^@)%@(�@'��@'|�@'|�@'\)@';d@'�@&�y@&�@&�R@%?}@$�@$z�@$I�@$9X@$�@$1@#�
@#��@#�@#t�@#t�@#t�@#�@#"�@#@"�\@"^5@"M�@"=q@"�@!�#@!��@!x�@!X@!%@ ��@ �`@ ��@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ ��@ �u@ �@ �@ bN@�@|�@\)@K�@;d@+@�y@��@�+@v�@v�@v�@ff@ff@ff@V@V@E�@5?@$�@$�@{@�T@@��@�@�@�j@��@�D@Z@9X@�@��@�
@��@dZ@�@��@��@�\@n�@-@��@��@�@�@�#@�^@��@X@b@��@��@��@�+@�+@v�@E�@{@�T@�-@��@��@�@�@`B@O�@�@z�@I�@�@1@�m@t�@"�@�H@^5@J@�^@X@&�@�u@bN@A�@�@�@��@\)@ȴ@��@v�@ff@V@$�@�@@�-@��@�h@p�@?}@/@/@V@�@�@�D@z�@j@Z@Z@Z@Z@Z@I�@(�@�@1@1@��@��@��@S�@33@o@
�!@
�\@
n�@
^5@
^5@
M�@
^5@
^5@
^5@
^5@
^511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�p�A�r�A�ZA���A�jA�Q�A�bNA��A̛�A�n�A�$�A�ĜA�jA���A�=qA�|�A�\)A�1A��7A��A��7A���A�33A�p�A���A��HA���A��A�Q�A���A��A� �A��\A�ZA�A�1'A�|�A�%A��!A�C�A�-A���A��PA�jA�|�A�+A��A�ƨA���A�dZA���A�v�A�I�A�7LA���A�\)A�"�A��^A�VA�A�ĜA��hA�`BA��A��`A�ƨA��-A�A�A��hA�ZA��-A��FA�|�A��9A��A��A��PA�9XA�ZA��
A���A�p�A�%A�ffA�33A�A�bNA�XA��wA�A��7A�7LA���A��A�ȴA��wA��-A��FA��A�x�A�`BA�G�A�-A��A���A���A��A��A��A�33A��HA�\)A�  A�"�A�FA�hA�A~�A|��Azv�Ax=qAw�Av�Au/At9XAr�uAp��Ap{AnbNAmVAk�PAi/Ag�Ae�PAeoAd��AcƨAb�jAb{AaA]��AZ��AY�FAYl�AYdZAY\)AY?}AY;dAY&�AX�AW33AU33ATv�AS�AR�APbNAOdZAL�AKdZAJĜAH��AF�AF^5AE��AE"�AD~�AD(�AC�hAAƨA@�DA?�
A?"�A>A�A<��A;��A;�7A;XA:1'A8��A7l�A7?}A6�DA6�A57LA3�mA1�hA1
=A0�A09XA/��A.��A-�A-`BA-
=A+��A+|�A+;dA*��A)�PA(�A(ZA(=qA(�A'�mA'&�A&~�A&bA%�FA%;dA$��A#�#A#VA"��A"�+A!?}A��A�A��A+A-A��AA��AoAXA~�A1'AS�A�!A�AJA~�AC�A��A	ƨA��A��A��A��A��A�
AdZAK�A"�A^5A�A�#At�A b@�J@�z�@���@��@���@�@�n�@�|�@���@�&�@�dZ@�hs@�@�  @�\)@�-@�@۝�@���@���@ׅ@�@�?}@�  @�|�@�dZ@�K�@�n�@���@�|�@ϝ�@�|�@�V@���@ɉ7@�bN@�(�@��
@ǅ@�dZ@�"�@�ff@��@�r�@�ƨ@�t�@�
=@�ff@���@�j@�b@��H@�-@�{@�$�@�n�@�^5@��-@���@�;d@�~�@�v�@�n�@�V@�r�@���@�?}@��@��j@��u@�9X@���@���@��^@��j@�(�@��@���@��7@�7L@��@�O�@�G�@��@��j@�9X@�\)@�"�@��!@��+@�=q@���@�G�@��@�b@��@��w@��@�ff@���@��-@��@���@�r�@�Q�@�(�@��@��@�|�@���@��@�&�@�z�@�b@���@�K�@�@���@�~�@��^@�G�@�7L@��j@�Q�@�I�@� �@�b@�ƨ@��@�t�@�C�@��@���@���@�V@��@�7L@�%@���@���@���@�j@�1'@��;@�l�@���@���@��h@�x�@�&�@��@�I�@� �@�A�@��
@�|�@�+@��y@��R@��\@�V@�$�@��#@���@�G�@�%@��9@��u@�j@�A�@�1'@�b@�1@�1@��;@�ƨ@��w@���@�dZ@�;d@��@�M�@���@���@��@���@��@�r�@�j@�I�@� �@���@�;d@��H@�~�@�v�@�{@��T@���@�`B@�&�@��j@�A�@��m@���@�l�@�C�@�"�@��y@�^5@�J@��-@��@�/@��`@���@�r�@�Z@�Q�@�Q�@��;@��@�C�@��@��y@���@���@�v�@�5?@���@��-@���@��7@�X@��@���@��@���@��u@��D@��@�I�@�1@�;@�w@�w@�@�@�@|�@~��@~��@~ff@~@}�-@}�h@}O�@|��@|�j@|(�@{"�@z�@z�\@z�@y�^@y��@yhs@x�`@y�@y&�@x�@w�@wl�@w;d@v�y@vff@u��@uO�@t��@tj@tj@tI�@t9X@t�@s��@s�
@s�F@r�!@q�#@q��@q7L@p�`@pr�@o�@o��@o�@n��@nV@m?}@l�D@lj@lZ@l9X@l�@l�@l�@l1@k�
@kt�@k33@k"�@k@j��@j�\@jM�@i��@i%@h��@hr�@hA�@g�@g�@g
=@e��@eO�@eV@dI�@c�m@c��@c��@b�\@b-@a��@ahs@`Q�@`1'@`1'@`b@_�;@_�w@_�@_�@_�@_��@_�P@_K�@^V@^@]�@]��@]�-@]�h@]�h@]/@\�j@\j@\(�@\�@[�
@[�@[C�@Z�H@Z~�@Z^5@ZJ@Y�@Y��@Y��@YX@Y&�@X��@X��@W�@Vff@U�@U��@U�-@U��@U�h@U�@T�j@T�j@Tz�@T1@S��@SS�@R�@R�\@RJ@Q��@Q&�@P��@PĜ@PbN@O|�@Nȴ@N�+@NE�@M�@M/@L�j@K��@Kƨ@KS�@Ko@J��@J^5@I��@I��@Ihs@I&�@I%@H�u@G�;@G�@G��@G��@Gl�@G\)@G;d@G
=@Fȴ@F��@F��@Fv�@FE�@F@E@E�@EO�@EV@Dj@C��@C��@CS�@C"�@B�H@B�\@A�7@AG�@A&�@@��@@��@@��@@bN@@A�@?�@?l�@?
=@>v�@=�T@=�h@=?}@=/@=/@<�/@<9X@<1@;�m@;�F@;S�@:��@:=q@:�@9�@9��@9G�@9%@8�u@7�@7��@7�@7|�@7K�@7+@7+@7+@7+@7+@7+@7+@7
=@7
=@6�y@6�@6$�@5�@4�/@4�/@4�/@4��@4��@4�@4�D@49X@3�
@3��@3��@3t�@3C�@333@3"�@3o@2�@2�H@2�H@2�H@2�\@2^5@2M�@2M�@2-@2-@2-@2J@1�#@1�^@1�7@0Ĝ@0Ĝ@0��@01'@/��@/|�@/l�@/\)@/+@/+@/�@.�R@-�T@-`B@-/@-�@-�@-V@,�@,��@,�D@,�D@,z�@,Z@,I�@,(�@+�
@+t�@+dZ@+S�@+S�@+"�@*�\@*M�@)�^@)%@(�@'��@'|�@'|�@'\)@';d@'�@&�y@&�@&�R@%?}@$�@$z�@$I�@$9X@$�@$1@#�
@#��@#�@#t�@#t�@#t�@#�@#"�@#@"�\@"^5@"M�@"=q@"�@!�#@!��@!x�@!X@!%@ ��@ �`@ ��@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ Ĝ@ ��@ �u@ �@ �@ bN@�@|�@\)@K�@;d@+@�y@��@�+@v�@v�@v�@ff@ff@ff@V@V@E�@5?@$�@$�@{@�T@@��@�@�@�j@��@�D@Z@9X@�@��@�
@��@dZ@�@��@��@�\@n�@-@��@��@�@�@�#@�^@��@X@b@��@��@��@�+@�+@v�@E�@{@�T@�-@��@��@�@�@`B@O�@�@z�@I�@�@1@�m@t�@"�@�H@^5@J@�^@X@&�@�u@bN@A�@�@�@��@\)@ȴ@��@v�@ff@V@$�@�@@�-@��@�h@p�@?}@/@/@V@�@�@�D@z�@j@Z@Z@Z@Z@Z@I�@(�@�@1@1@��@��@��@S�@33@o@
�!@
�\@
n�@
^5@
^5@
M�@
^5@
^5@
^5@
^5@
^511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
�B
��BJBhBbBbB�B>wBM�B��B��B�B�B!�B0!B?}BbNBq�Bz�B�hB�B��B��B�wB�LBB�qB�wBƨBŢB�wB��B�B�3B�^BŢBŢBŢBÖB�qB�FB�?B�3B�-B�3B�9B�LB�FB�FB�FB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�Br�B[#BN�BI�BE�BC�B?}B/B�B�B\B
=BB��B�#BŢB�?B��B��B��B�bB�DB�Bw�B[#B@�B2-B#�B�BuBhB\BPB%B
��B
�sB
�;B
�)B
�B
��B
��B
�XB
�-B
��B
�\B
�PB
�=B
�B
{�B
n�B
_;B
YB
O�B
H�B
@�B
7LB
,B
&�B
�B
uB
1B	��B	�B	�NB	�;B	�)B	�
B	��B	��B	B	�!B	��B	��B	��B	��B	��B	�{B	�uB	�hB	�VB	�B	y�B	v�B	s�B	l�B	dZB	]/B	P�B	F�B	B�B	7LB	/B	1'B	1'B	+B	'�B	%�B	!�B	�B	uB	\B	DB	+B	  B��B��B��B�B�B�ZB�TB�TB�HB�/B�
BɺBȴBɺBǮBŢB��B�jB�XB�FB�3B�FB�LB�XB�LB�?B�?B�9B�3B�-B�B�B��B��B��B��B��B��B��B��B��B�\B�=B�+B�B�B~�B|�Bz�Bv�Bs�Bq�Bo�Bm�Bk�BgmBcTB`BB^5BZBYBW
BT�BS�BR�BQ�BQ�BP�BP�BN�BN�BM�BL�BK�BK�BI�BG�BD�BF�BF�BI�BG�BJ�BH�BF�BD�BD�BD�BC�BC�BC�BF�BH�BI�BI�BN�BO�BM�BP�BQ�BR�BR�BXB_;BaHBjBm�BjBe`BaHBbNBcTBdZBdZBffBgmBiyBl�Bm�Bo�Bo�Bo�Bo�Bt�Bu�Bv�Bz�B}�B~�B� B�B�VB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�qB��B��BBÖBǮB��B��B�B�B�5B�BB�BB�`B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	%B	%B	
=B	bB	�B	�B	�B	 �B	#�B	%�B	'�B	(�B	+B	1'B	6FB	6FB	:^B	<jB	<jB	@�B	G�B	K�B	N�B	O�B	P�B	Q�B	R�B	T�B	VB	VB	YB	ZB	[#B	[#B	\)B	\)B	]/B	]/B	`BB	_;B	]/B	]/B	]/B	]/B	^5B	_;B	_;B	_;B	_;B	_;B	`BB	aHB	cTB	ffB	iyB	jB	l�B	n�B	q�B	u�B	x�B	z�B	}�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�JB	�VB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�FB	�LB	�XB	�^B	�jB	�wB	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�NB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
DB
JB
JB
PB
VB
\B
\B
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
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
,B
,B
,B
,B
-B
-B
-B
,B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
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
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
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
C�B
C�B
C�B
C�B
C�B
D�B
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
F�B
F�B
F�B
G�B
H�B
I�B
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
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
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
S�B
T�B
S�B
T�B
T�B
T�B
VB
T�B
T�B
T�B
VB
VB
T�B
T�B
VB
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
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
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
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
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
l�B
l�B
m�B
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
q�B
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
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
�B
��BJBhBbBbB�B>wBM�B��B��B�B�B!�B0!B?}BbNBq�Bz�B�hB�B��B��B�wB�LBB�qB�wBƨBŢB�wB��B�B�3B�^BŢBŢBŢBÖB�qB�FB�?B�3B�-B�3B�9B�LB�FB�FB�FB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�Br�B[#BN�BI�BE�BC�B?}B/B�B�B\B
=BB��B�#BŢB�?B��B��B��B�bB�DB�Bw�B[#B@�B2-B#�B�BuBhB\BPB%B
��B
�sB
�;B
�)B
�B
��B
��B
�XB
�-B
��B
�\B
�PB
�=B
�B
{�B
n�B
_;B
YB
O�B
H�B
@�B
7LB
,B
&�B
�B
uB
1B	��B	�B	�NB	�;B	�)B	�
B	��B	��B	B	�!B	��B	��B	��B	��B	��B	�{B	�uB	�hB	�VB	�B	y�B	v�B	s�B	l�B	dZB	]/B	P�B	F�B	B�B	7LB	/B	1'B	1'B	+B	'�B	%�B	!�B	�B	uB	\B	DB	+B	  B��B��B��B�B�B�ZB�TB�TB�HB�/B�
BɺBȴBɺBǮBŢB��B�jB�XB�FB�3B�FB�LB�XB�LB�?B�?B�9B�3B�-B�B�B��B��B��B��B��B��B��B��B��B�\B�=B�+B�B�B~�B|�Bz�Bv�Bs�Bq�Bo�Bm�Bk�BgmBcTB`BB^5BZBYBW
BT�BS�BR�BQ�BQ�BP�BP�BN�BN�BM�BL�BK�BK�BI�BG�BD�BF�BF�BI�BG�BJ�BH�BF�BD�BD�BD�BC�BC�BC�BF�BH�BI�BI�BN�BO�BM�BP�BQ�BR�BR�BXB_;BaHBjBm�BjBe`BaHBbNBcTBdZBdZBffBgmBiyBl�Bm�Bo�Bo�Bo�Bo�Bt�Bu�Bv�Bz�B}�B~�B� B�B�VB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�qB��B��BBÖBǮB��B��B�B�B�5B�BB�BB�`B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	%B	%B	
=B	bB	�B	�B	�B	 �B	#�B	%�B	'�B	(�B	+B	1'B	6FB	6FB	:^B	<jB	<jB	@�B	G�B	K�B	N�B	O�B	P�B	Q�B	R�B	T�B	VB	VB	YB	ZB	[#B	[#B	\)B	\)B	]/B	]/B	`BB	_;B	]/B	]/B	]/B	]/B	^5B	_;B	_;B	_;B	_;B	_;B	`BB	aHB	cTB	ffB	iyB	jB	l�B	n�B	q�B	u�B	x�B	z�B	}�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�JB	�VB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�FB	�LB	�XB	�^B	�jB	�wB	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�NB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
DB
JB
JB
PB
VB
\B
\B
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
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
,B
,B
,B
,B
-B
-B
-B
,B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
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
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
@�B
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
C�B
C�B
C�B
C�B
C�B
D�B
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
F�B
F�B
F�B
G�B
H�B
I�B
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
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
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
S�B
T�B
S�B
T�B
T�B
T�B
VB
T�B
T�B
T�B
VB
VB
T�B
T�B
VB
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
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
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
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
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
l�B
l�B
m�B
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
q�B
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
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20180615170244                              AO  ARCAADJP                                                                    20180615170244    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180615170244  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180615170244  QCF$                G�O�G�O�G�O�0               